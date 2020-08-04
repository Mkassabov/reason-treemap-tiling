//* based on https://pdfs.semanticscholar.org/c014/7b41048540b8cb461da8216cbc565dca050e.pdf

type rect = {
  x0: float,
  y0: float,
  x1: float,
  y1: float,
};

type datum = {
  value: float,
  label: string,
};

type tree =
  | Leaf(datum)
  | Branch(list(tree));

type cell = {
  value: float,
  label: string,
  layout: rect,
};

let getShorterSide = rect => {
  let width = abs_float(rect.x1 -. rect.x0);
  let height = abs_float(rect.y1 -. rect.y0);
  min(width, height);
};

let sum = list => List.fold_left((acc, cur) => acc +. cur, 0.0, list);

let rec sumValuesInTree = node =>
  switch (node) {
  | Leaf(datum) => datum.value
  | Branch(subTrees) =>
    let subTreeValue = List.map(sumValuesInTree, subTrees);
    sum(subTreeValue);
  };

let rec maxOfList = (head, rest) =>
  switch (rest) {
  | [] => head
  | [headRest, ...restRest] => max(head, maxOfList(headRest, restRest))
  };

let rec minOfList = (head, rest) =>
  switch (rest) {
  | [] => head
  | [headRest, ...restRest] => min(head, minOfList(headRest, restRest))
  };

let maxRowAspectRatio = (head, rest, width) => {
  //* using the following formula:
  //* max(((( w^2 * r_plus )) / ( s^2 )), (( s^2 ) / ( w^2 * r_minus )))
  let rPlus = maxOfList(head, rest);
  let rMinus = minOfList(head, rest);
  let s = sum([head, ...rest]);

  let operandPlus = width *. width *. rPlus /. (s *. s);
  let operandMinus = s *. s /. (width *. width *. rMinus);
  max(operandPlus, operandMinus);
};

let fitRowInContainer = (row: list((tree, float)), container) => {
  let width = abs_float(container.x1 -. container.x0);
  let height = abs_float(container.y1 -. container.y0);
  let rowValues = List.map(((_, value)) => value, row);
  let rowAreaSum = sum(rowValues);
  let containerArea = width *. height;

  let occupiedFraction = rowAreaSum /. containerArea;

  if (width <= height) {
    //* container is taller than wide do vertical layout
    //* divider line between noder and remainder of grid
    let newY0 = container.y1 -. occupiedFraction *. height;

    let remainingGrid = {
      x0: container.x0,
      y0: container.y0,
      x1: container.x1,
      y1: newY0,
    };

    //* layout nodes vertically
    let (nodesWithLayout, _) =
      List.fold_left(
        (acc, cur) => {
          let (result, prevX1) = acc;
          let (next, nextValue) = cur;
          let nextWidth = nextValue /. rowAreaSum *. width;
          let nextX1 = prevX1 +. nextWidth;
          let nextRect = {
            x0: prevX1,
            y0: newY0,
            x1: nextX1,
            y1: container.y1,
          };
          let nextAcc = ([(next, nextRect), ...result], nextX1);
          nextAcc;
        },
        ([], container.x0),
        row,
      );
    (nodesWithLayout, remainingGrid);
  } else {
    //* container is wider than tall do horizontal layout
    //* divider line between noder and remainder of grid
    let newX1 = container.x0 +. occupiedFraction *. width;

    let remainingGrid = {
      x0: newX1,
      y0: container.y0,
      x1: container.x1,
      y1: container.y1,
    };

    //* layout nodes horizontally
    let (nodesWithLayout, _) =
      List.fold_left(
        (acc, cur) => {
          let (result, prevY0) = acc;
          let (next, nextValue) = cur;
          let nextHeight = nextValue /. rowAreaSum *. height;
          let nextY0 = prevY0 +. nextHeight;
          let nextRect = {
            x0: container.x0,
            y0: nextY0,
            x1: newX1,
            y1: prevY0,
          };
          let nextAcc = ([(next, nextRect), ...result], nextY0);
          nextAcc;
        },
        ([], container.y1),
        row,
      );
    (nodesWithLayout, remainingGrid);
  };
};

let rec squarify = (remaining, currentRow, container, result) => {
  switch (remaining, currentRow) {
  //* case: no more nodes -> finished
  | ([], []) => result

  //* case: no more nodes to lay, add last row -> case: no more nodes
  | ([], [headCurrentRow, ...restCurrentRow]) =>
    let currentRowHaveValues =
      List.map(
        node => (node, sumValuesInTree(node)),
        [headCurrentRow, ...restCurrentRow],
      );
    let (nodesHaveLayout, remainingRect) =
      fitRowInContainer(currentRowHaveValues, container);
    squarify([], [], remainingRect, List.concat([nodesHaveLayout, result]));

  //* case: start new empty row -> put first remaining node into row
  | ([headRemaing, ...restRemaining], []) =>
    squarify(restRemaining, [headRemaing], container, result)

  //* case: general case
  | ([headRemaing, ...restRemaining], [headCurrentRow, ...restCurrentRow]) =>
    let shortSide = getShorterSide(container);

    //* calculate max aspect ratio
    let rowValues = List.map(sumValuesInTree, restCurrentRow);
    let currentMaxAspectRatio =
      maxRowAspectRatio(
        sumValuesInTree(headCurrentRow),
        rowValues,
        shortSide,
      );

    //* calculate potential max aspect ratio
    let potentialRowValue =
      List.map(sumValuesInTree, [headCurrentRow, ...restCurrentRow]);
    let potentialMaxAspectRatio =
      maxRowAspectRatio(
        sumValuesInTree(headRemaing),
        potentialRowValue,
        shortSide,
      );

    if (potentialMaxAspectRatio < currentMaxAspectRatio) {
      //* add next node to current row to reduce aspect ratio
      squarify(
        restRemaining,
        List.concat([[headCurrentRow, ...restCurrentRow], [headRemaing]]),
        container,
        result,
      );
    } else {
      //* add next node to current row will increase
      //* aspect ratio -> case: start new empty row
      let currentRowHaveValues =
        List.map(
          node => (node, sumValuesInTree(node)),
          [headCurrentRow, ...restCurrentRow],
        );
      let (nodesWithLayout, remainingRect) =
        fitRowInContainer(currentRowHaveValues, container);
      squarify(
        [headRemaing, ...restRemaining],
        [],
        remainingRect,
        List.concat([nodesWithLayout, result]),
      );
    };
  };
};

let rec traverse = (input: tree, container: rect): list(cell) => {
  switch (input) {
  | Leaf({value, label}) => [{value, label, layout: container}]
  | Branch(subTrees) =>
    let nodesWithLayout = squarify(subTrees, [], container, []);
    List.concat(
      List.map(
        ((tree, innerContainer)) => traverse(tree, innerContainer),
        nodesWithLayout,
      ),
    );
  };
};

let testData =
  Branch([
    Leaf({value: 6.0, label: "Container 0"}),
    Branch([
      Branch([
        Leaf({value: 3.0, label: "Container 1.1.1"}),
        Leaf({value: 1.0, label: "Container 1.1.2"}),
      ]),
      Leaf({value: 2.0, label: "Container 1.2"}),
    ]),
    Leaf({value: 4.0, label: "Container 2"}),
    Leaf({value: 3.0, label: "Container 3"}),
    Leaf({value: 2.0, label: "Container 4"}),
    Leaf({value: 2.0, label: "Container 5"}),
    Leaf({value: 1.0, label: "Container 6"}),
  ]);

let testContainer = {x0: 0.0, y0: 0.0, x1: 6.0, y1: 4.0};

let out = Array.of_list(traverse(testData, testContainer));
[%raw "console.dir(out,{depth: null})"];
