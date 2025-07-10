:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).

% Dynamic clause storage
:- dynamic dynamic_clause/1.

% API Endpoints
% GET  /hello        - Health check endpoint
% POST /assert       - Add a new Prolog clause
% POST /retract      - Remove a Prolog clause
% GET  /list_clauses - List all dynamic clauses
% GET  /query        - Execute a Prolog query

% handle_hello used to check server readiness after start-up
:- http_handler(root(hello), handle_hello, []).

% Clause assertion
:- http_handler(root(assert), handle_assert, [method(post)]).

% Clause retraction
:- http_handler(root(retract), handle_retract, [method(post)]).

% Clause listing
:- http_handler(root(list_clauses), handle_list_clauses, [method(get)]).

% Query handling
:- http_handler(root(query), handle_query, [method(get)]).

% Demo endpoint
handle_hello(_Request) :-
    reply_json(json([status=hello])).

% 1. Add a clause
handle_assert(Request) :-
    http_read_json_dict(Request, Dict),
    ClauseStr = Dict.get(clause),
    term_string(Term, ClauseStr),
    assertz(Term),
    assertz(dynamic_clause(ClauseStr)),  % Store the clause in dynamic storage
    reply_json_dict(_{status:success, asserted:ClauseStr}).

% 2. Remove a clause
handle_retract(Request) :-
    http_read_json_dict(Request, Dict),
    ClauseStr = Dict.get(clause),
    term_string(Term, ClauseStr),  % Convert the clause text back into a Prolog term
    retractall(Term),              % Remove the actual Prolog clause
    retractall(dynamic_clause(ClauseStr)),
    reply_json_dict(_{status:success, removed:ClauseStr}).

% 3. List all clauses
handle_list_clauses(_Request) :-
    findall(Clause, dynamic_clause(Clause), Clauses),
    reply_json(Clauses).

% Handle queries: expects 'q' parameter, runs it, and returns JSON results with variable bindings
handle_query(Request) :-
    catch(
        (   http_parameters(Request, [q(QueryString, [string])]),
            term_string(Term, QueryString),  % Convert string to Prolog term
            findall(Term, call(Term), Solutions),
            (   Solutions = []
            ->  reply_json_dict(_{result: false, message: "No solutions found"})
            ;   Solutions = [_|_]
            ->  format_solutions(Term, Solutions, FormattedSolutions),
                reply_json_dict(_{result: success, solutions: FormattedSolutions})
            )
        ),
        Error,
        (   % Handle errors gracefully
            format(string(ErrorString), '~w', [Error]),  % Convert error to string
            reply_json_dict(_{result: error, message: ErrorString})  % Return error as JSON
        )
    ).

% Format solutions by converting terms to strings
format_solutions(_, [], []).
format_solutions(Template, [Solution|Rest], [SolutionStr|FormattedRest]) :-
    format(string(SolutionStr), '~w', [Solution]),
    format_solutions(Template, Rest, FormattedRest).


% Start the HTTP server
start_server :-
    (getenv('PORT', PortStr) -> atom_number(PortStr, Port)
    ; current_prolog_flag(argv, [PortArg|_]) -> atom_number(PortArg, Port)
    ; Port = 8080),  % Default to 8080
    format('Server running at http://localhost:~w/hello~n', [Port]),
    http_server([port(Port)]),
    thread_get_message(_).  % Block forever

% Ensure server starts on load
:- initialization(start_server).