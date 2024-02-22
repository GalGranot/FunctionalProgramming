-module(shapes).
-export([shapesFilter/1]).
-export([shapesFilter2/1]).
-export([shapesArea/1]).
-export([squaresArea/1]).
-export([trianglesArea/1]).

shapesFilter(FilterName) when FilterName == rectangle; FilterName == ellipse; FilterName == triangle ->
    fun({shapes, ShapesList}) ->
        {shapes, [Shape || Shape = {Name, _} <- ShapesList, FilterName == Name]}
    end.

shapesFilter2(FilterName) when FilterName == rectangle; FilterName == ellipse; FilterName == triangle; FilterName == square; FilterName == circle ->
    fun({shapes, ShapesList}) ->
        {shapes, [Shape || Shape = {Name, _} <- ShapesList, FilterName == Name]}
    end.

shapesArea({shapes, []}) -> 0;
shapesArea({shapes, [HeadShape|ListTail]}) ->
    area(HeadShape) + shapesArea({shapes, ListTail}).

squaresArea({shapes, ShapesList}) ->
    SquareFilter = shapesFilter2(square),
    FilteredList = SquareFilter({shapes, ShapesList}),
    shapesArea(FilteredList).

trianglesArea({shapes, ShapesList}) ->
    TriangleFilter = shapesFilter(triangle),
    FilteredList = TriangleFilter({shapes, ShapesList}),
    shapesArea(FilteredList).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Private Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
area({rectangle, {dim, Width, Height}}) when Width > 0, Height > 0 -> 
    Width * Height;
area({triangle, {dim, Base, Height}}) when Base > 0, Height > 0 -> 
    0.5 * Base * Height;
area({ellipse, {radius, Radius1, Radius2}}) when Radius1 > 0, Radius2 >0 -> 
    math:pi() * Radius1 * Radius2.