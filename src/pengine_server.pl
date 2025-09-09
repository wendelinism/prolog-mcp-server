% Pengine Server for Multi-User Prolog Sessions
%
% Commands that work:
% 1. Start pengine server:
%    PORT=9998 swipl -g "start_pengine_server" -t "halt" src/pengine_server.pl
%
% 2a. Test local basic pengine creation, not on server (works):
%    swipl -g "use_module(library(pengines)), pengine_create([id(PengineID), sandbox(true)]), write('Created pengine: '), write(PengineID), nl, halt"
%
% 2b. Test basic pengine creation on that server created in 1, and keep interactive session open(works):
% swipl -g "use_module(library(pengines)), pengine_create([server('http://localhost:9998'), id(PengineID), sandbox(true)]), write('Created pengine: '), write(PengineID), nl"
%
% 3. Create pengine/session via HTTP (works):
%    curl -X POST -H "Content-Type: application/json" -d '{"action": "create"}' http://localhost:9998/session
%
% 4. Test whether pengine is alive (always shown dead currently, so either pengine or request fails)
% curl "http://localhost:9998/pengine_status?pengine_id=YOUR_PENGINE_ID"
%
% 5. Test clause assertion (currently fails):
%    curl -X POST -H "Content-Type: application/json" -d '{"session_id": "SESSION_ID", "clause": "parent(tom, bob)."}' http://localhost:9998/assert
%
% Known issues:
% - pengine_ask() and pengine_send() fail in HTTP handler context
% - Direct pengine interaction from server context doesn't work
% - Need to use built-in pengine HTTP endpoints or find proper server API

:- use_module(library(pengines)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uuid)).

% Enable pengines - use default application
% :- pengine_application(prolog_mcp).

% Dynamic clause storage per session
:- dynamic session_clause/2.  % session_clause(SessionID, Clause)

% Session management
:- dynamic session_pengine/2.  % session_pengine(SessionID, PengineID)

% API Endpoints
:- http_handler(root(session), handle_session, [method(post)]).
:- http_handler(root(assert), handle_assert, [method(post)]).
:- http_handler(root(retract), handle_retract, [method(post)]).
:- http_handler(root(list_clauses), handle_list_clauses, [method(get)]).
:- http_handler(root(query), handle_query, [method(get)]).
:- http_handler(root(cleanup), handle_cleanup, [method(post)]).
:- http_handler(root(pengine_status), handle_pengine_status, [method(get)]).
:- http_handler(root(start_long_task), handle_start_long_task, [method(post)]).

% Session creation/management
% Debug logging helper
debug_log(Msg) :-
    open('/tmp/pengine_debug.log', append, Stream),
    get_time(Time),
    format_time(atom(TimeStr), '%Y-%m-%d %H:%M:%S', Time),
    format(Stream, '[~w] ~w~n', [TimeStr, Msg]),
    close(Stream).

handle_session(Request) :-
    debug_log('Session handler called'),
    catch(
        http_read_json_dict(Request, Dict),
        Error,
        (debug_log(json_parsing_failed(Error)), throw(Error))
    ),
    debug_log(parsed_json(Dict)),
    (   Dict.get(action) == "create"
    ->  (debug_log('Creating session'),
         create_user_session(SessionID, PengineID),
         reply_json(_{status:success, session_id:SessionID, pengine_id:PengineID}))
    ;   Dict.get(action) == "destroy"
    ->  (debug_log('Destroying session'),
         SessionID = Dict.get(session_id),
         destroy_user_session(SessionID),
         reply_json(_{status:success, message:"Session destroyed"}))
    ;   (debug_log(invalid_action(Dict.get(action))),
         reply_json(_{status:error, message:"Invalid action"}))
    ).

% Create isolated pengine for user session using standard pengine HTTP API
create_user_session(SessionID, PengineID) :-
    uuid(SessionID),
    debug_log(creating_session(SessionID)),
    
    % Use standard pengine creation - this will create a pengine with proper HTTP handling
    % Add explicit idle timeout of 300 seconds
    catch(
        pengine_create([
            id(PengineID),
            sandbox(true),
            idle_timeout(300)   % 300 seconds idle timeout
        ]),
        Error,
        (debug_log(pengine_creation_failed(Error)), throw(Error))
    ),
    
    debug_log(pengine_created(PengineID)),
    
    % Check if pengine is actually alive immediately after creation
    (   pengine_property(PengineID, self(PengineID))
    ->  debug_log(pengine_alive_after_creation(PengineID))
    ;   debug_log(pengine_dead_immediately(PengineID))
    ),
    
    assertz(session_pengine(SessionID, PengineID)),
    debug_log(session_mapped(SessionID, PengineID)),
    
    % Immediately verify the mapping was stored
    (   session_pengine(SessionID, PengineID)
    ->  debug_log(session_mapping_verified(SessionID, PengineID))
    ;   debug_log(session_mapping_failed_to_store(SessionID, PengineID))
    ).


% Cleanup user session
destroy_user_session(SessionID) :-
    (   session_pengine(SessionID, PengineID)
    ->  pengine_destroy(PengineID),
        retractall(session_pengine(SessionID, _)),
        retractall(session_clause(SessionID, _))
    ;   true
    ).

handle_pengine_status(Request) :-
    http_parameters(Request, [pengine_id(PID, [string])]),
    debug_log(checking_pengine_status(PID)),
    
    % Check pengine using multiple methods for better debugging
    (   pengine_property(PID, self(PID))
    ->  Method1 = alive
    ;   Method1 = dead
    ),
    
    % Try alternative check - see if we can get any properties
    (   catch(pengine_property(PID, _), _, fail)
    ->  Method2 = has_properties  
    ;   Method2 = no_properties
    ),
    
    % Check our session mapping (fix the bug - use _ instead of unbound SessionID)
    (   session_pengine(_, PID)
    ->  SessionMapping = found
    ;   SessionMapping = not_found
    ),
    
    debug_log(pengine_status_check(PID, method1=Method1, method2=Method2, session_mapping=SessionMapping)),
    
    (   Method1 = alive
    ->  reply_json(_{status:alive, pengine_id:PID, method1:Method1, method2:Method2, session_mapping:SessionMapping})
    ;   reply_json(_{status:dead, pengine_id:PID, method1:Method1, method2:Method2, session_mapping:SessionMapping})
    ).



% Get pengine ID for session
get_session_pengine(Request, SessionID, PengineID) :-
    (   http_parameters(Request, [session_id(SessionID, [string])])
    ->  true
    ;   memberchk(session_id=SessionID, Request)
    ),
    (   session_pengine(SessionID, PengineID)
    ->  true
    ;   throw(error(session_not_found, SessionID))
    ).

% Add clause to user's pengine  
handle_assert(Request) :-
    catch(
        (   http_read_json_dict(Request, Dict),
            SessionID = Dict.get(session_id),
            ClauseStr = Dict.get(clause),
            session_pengine(SessionID, PengineID),
            term_string(Term, ClauseStr),
            pengine_send(PengineID, ask(assertz(Term), [])),
            assertz(session_clause(SessionID, ClauseStr)),
            reply_json(_{status:success, asserted:ClauseStr})
        ),
        Error,
        (   format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% Remove clause from user's pengine
handle_retract(Request) :-
    catch(
        (   http_read_json_dict(Request, Dict),
            SessionID = Dict.get(session_id),
            ClauseStr = Dict.get(clause),
            session_pengine(SessionID, PengineID),
            term_string(Term, ClauseStr),
            pengine_send(PengineID, ask(retractall(Term), [])),
            retractall(session_clause(SessionID, ClauseStr)),
            reply_json(_{status:success, removed:ClauseStr})
        ),
        Error,
        (   format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% List clauses for user's pengine
handle_list_clauses(Request) :-
    catch(
        (   get_session_pengine(Request, SessionID, _PengineID),
            findall(Clause, session_clause(SessionID, Clause), Clauses),
            reply_json(Clauses)
        ),
        Error,
        (   format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% Execute query in user's pengine
handle_query(Request) :-
    catch(
        (   get_session_pengine(Request, _SessionID, PengineID),
            http_parameters(Request, [q(QueryString, [string])]),
            term_string(Term, QueryString),
            pengine_send(PengineID, ask(Term, [])),
            % For now, just return success - proper event handling would be needed for full implementation
            reply_json(_{result:success, message:"Query sent"})
        ),
        Error,
        (   format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% Session cleanup endpoint
handle_cleanup(Request) :-
    http_read_json_dict(Request, Dict),
    SessionID = Dict.get(session_id),
    destroy_user_session(SessionID),
    reply_json(_{status:success, message:"Session cleaned up"}).

% Start a long-running task to test pengine lifecycle
handle_start_long_task(Request) :-
    catch(
        (   http_read_json_dict(Request, Dict),
            SessionID = Dict.get(session_id),
            session_pengine(SessionID, PengineID),
            debug_log(starting_long_task(SessionID, PengineID)),
            % Start a task that counts and logs every 5 seconds for 2 minutes
            pengine_send(PengineID, ask(count_and_log(1, 24), [])),
            reply_json(_{status:success, message:"Long task started", pengine_id:PengineID})
        ),
        Error,
        (   debug_log(long_task_failed(Error)),
            format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% Predicate to count and log - will be sent to pengine
count_and_log(N, Max) :-
    N =< Max,
    get_time(Time),
    format('Task step ~w at time ~w~n', [N, Time]),
    sleep(5),
    N1 is N + 1,
    count_and_log(N1, Max).

% Collect all solutions from pengine
collect_solutions(PengineID, Solutions) :-
    collect_solutions(PengineID, [], Solutions).

collect_solutions(PengineID, Acc, Solutions) :-
    pengine_event(PengineID, Event),
    (   Event = success(Solution, _More)
    ->  collect_solutions(PengineID, [Solution|Acc], Solutions)
    ;   Event = failure
    ->  reverse(Acc, Solutions)
    ;   Event = error(Error)
    ->  throw(Error)
    ;   reverse(Acc, Solutions)
    ).

% Format solutions for JSON response
format_solutions([], []).
format_solutions([Solution|Rest], [SolutionStr|FormattedRest]) :-
    format(string(SolutionStr), '~w', [Solution]),
    format_solutions(Rest, FormattedRest).

% Start pengine-enabled server
start_pengine_server :-
    (getenv('PORT', PortStr) -> atom_number(PortStr, Port)
    ; current_prolog_flag(argv, [PortArg|_]) -> atom_number(PortArg, Port)
    ; Port = 8080),
    http_server([port(Port)]),
    thread_get_message(_).

% Initialize pengine server
:- initialization(start_pengine_server).