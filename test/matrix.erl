-module(matrix).
-export([getZeroMat/2, getRow/2, getCol/2, setElementMat/4, getSizes/2]).

% generate a matrix with X rows and Y columns with zeros
getZeroMat(X,Y) ->
list_to_tuple([list_to_tuple([0 || _Y <- lists:seq(1,Y)]) || _X <- lists:seq(1,X)]).

% return the ROW row of a Matrix in a tuple format
getRow(Mat, Row) ->
    element(Row, Mat).


% return the COL col of a Matrix in a tuple format
getCol(Mat,Col) ->
    list_to_tuple([element(Col,ColData) || ColData <- tuple_to_list(Mat)]).

% return a new Matrix which is a copy of OldMat with a NewVal as the value of Row,Col
setElementMat(Row,Col,OldMat, NewVal) ->
setelement (Row,OldMat ,setelement (Col,element(Row,OldMat),NewVal)).

getSizes(Mat1, Mat2)->
	{size(Mat1), size(Mat2), size(element(1,Mat2))}.