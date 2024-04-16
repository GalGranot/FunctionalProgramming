-module(q1).
-compile(export_all).

match (TeamA, TeamB)  -> TeamA,
    TeamB,
    ok.

playGames(Teams) ->
    [match(TeamA, TeamB) || TeamA <- Teams, TeamB <- Teams, TeamA /= TeamB].

getTeamResults(Matches, Team) ->
    getTeamResults(Matches, Team, 0).

getTeamResults([], _Team, CurrScore) ->
    CurrScore;

getTeamResults([H | T], Team, CurrScore) ->
    getTeamResults(T, Team, CurrScore + processGame(Team, H)).

processGame(Team, {result, TeamA, _TeamB, ScoreOfA, ScoreOfB}) ->
    case TeamA == Team of
        true -> MyScore = ScoreOfA, OppScore = ScoreOfB;
        false -> MyScore = ScoreOfB, OppScore = ScoreOfA
    end,
    score(MyScore, OppScore).

score(MyScore, MyScore) -> 1;
score(MyScore, OppScore) when OppScore > MyScore -> 0;
score(_MyScore, _OppScore) -> 3.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% allTeamsScore(Teams) ->
%     Matches = playGames(Teams),
%     [{Team, getTeamResults(Matches, Team)} || Team <- Teams].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allTeamsScore(Teams) ->
    GatherPid = spawn(gatherResults, [[], self(), 0, length(Teams)]),
    [spawn(getTeamResultsWrapper, [playGames(Teams), Team, GatherPid]) || Team <- Teams],
    receive
        List -> List
    end.

getTeamResultsWrapper(Matches, Team, GatherPid) ->
    GatherPid ! {Team, getTeamResults(Matches, Team)}.

gatherResults(ResultsList, ParentPid, JobsDone, JobsMax) ->
    receive
        {Team, Result} when JobsDone < JobsMax ->
            gatherResults([{Team, Result} | ResultsList], ParentPid, JobsDone + 1, JobsMax);
        {Team, Result} ->
            ParentPid ! [{Team, Result} | ResultsList]
    end.
