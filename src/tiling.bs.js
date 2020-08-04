// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");

function getShorterSide(rect) {
  var width = Math.abs(rect.x1 - rect.x0);
  var height = Math.abs(rect.y1 - rect.y0);
  if (width < height) {
    return width;
  } else {
    return height;
  }
}

function sum(list) {
  return List.fold_left((function (acc, cur) {
                return acc + cur;
              }), 0.0, list);
}

function sumValuesInTree(node) {
  if (node.tag) {
    return sum(List.map(sumValuesInTree, node[0]));
  } else {
    return node[0].value;
  }
}

function maxOfList(head, rest) {
  if (rest) {
    return Caml_obj.caml_max(head, maxOfList(rest[0], rest[1]));
  } else {
    return head;
  }
}

function minOfList(head, rest) {
  if (rest) {
    return Caml_obj.caml_min(head, minOfList(rest[0], rest[1]));
  } else {
    return head;
  }
}

function maxRowAspectRatio(head, rest, width) {
  var rPlus = maxOfList(head, rest);
  var rMinus = minOfList(head, rest);
  var s = sum(/* :: */[
        head,
        rest
      ]);
  var operandPlus = width * width * rPlus / (s * s);
  var operandMinus = s * s / (width * width * rMinus);
  if (operandPlus > operandMinus) {
    return operandPlus;
  } else {
    return operandMinus;
  }
}

function fitRowInContainer(row, container) {
  var width = Math.abs(container.x1 - container.x0);
  var height = Math.abs(container.y1 - container.y0);
  var rowValues = List.map((function (param) {
          return param[1];
        }), row);
  var rowAreaSum = sum(rowValues);
  var containerArea = width * height;
  var occupiedFraction = rowAreaSum / containerArea;
  if (width <= height) {
    var newY0 = container.y1 - occupiedFraction * height;
    var remainingGrid_x0 = container.x0;
    var remainingGrid_y0 = container.y0;
    var remainingGrid_x1 = container.x1;
    var remainingGrid = {
      x0: remainingGrid_x0,
      y0: remainingGrid_y0,
      x1: remainingGrid_x1,
      y1: newY0
    };
    var match = List.fold_left((function (acc, cur) {
            var prevX1 = acc[1];
            var nextWidth = cur[1] / rowAreaSum * width;
            var nextX1 = prevX1 + nextWidth;
            var nextRect_y1 = container.y1;
            var nextRect = {
              x0: prevX1,
              y0: newY0,
              x1: nextX1,
              y1: nextRect_y1
            };
            return /* tuple */[
                    /* :: */[
                      /* tuple */[
                        cur[0],
                        nextRect
                      ],
                      acc[0]
                    ],
                    nextX1
                  ];
          }), /* tuple */[
          /* [] */0,
          container.x0
        ], row);
    return /* tuple */[
            match[0],
            remainingGrid
          ];
  }
  var newX1 = container.x0 + occupiedFraction * width;
  var remainingGrid_y0$1 = container.y0;
  var remainingGrid_x1$1 = container.x1;
  var remainingGrid_y1 = container.y1;
  var remainingGrid$1 = {
    x0: newX1,
    y0: remainingGrid_y0$1,
    x1: remainingGrid_x1$1,
    y1: remainingGrid_y1
  };
  var match$1 = List.fold_left((function (acc, cur) {
          var prevY0 = acc[1];
          var nextHeight = cur[1] / rowAreaSum * height;
          var nextY0 = prevY0 + nextHeight;
          var nextRect_x0 = container.x0;
          var nextRect = {
            x0: nextRect_x0,
            y0: nextY0,
            x1: newX1,
            y1: prevY0
          };
          return /* tuple */[
                  /* :: */[
                    /* tuple */[
                      cur[0],
                      nextRect
                    ],
                    acc[0]
                  ],
                  nextY0
                ];
        }), /* tuple */[
        /* [] */0,
        container.y1
      ], row);
  return /* tuple */[
          match$1[0],
          remainingGrid$1
        ];
}

function squarify(_remaining, _currentRow, _container, _result) {
  while(true) {
    var result = _result;
    var container = _container;
    var currentRow = _currentRow;
    var remaining = _remaining;
    if (remaining) {
      var restRemaining = remaining[1];
      var headRemaing = remaining[0];
      if (currentRow) {
        var restCurrentRow = currentRow[1];
        var headCurrentRow = currentRow[0];
        var shortSide = getShorterSide(container);
        var rowValues = List.map(sumValuesInTree, restCurrentRow);
        var currentMaxAspectRatio = maxRowAspectRatio(sumValuesInTree(headCurrentRow), rowValues, shortSide);
        var potentialRowValue = List.map(sumValuesInTree, /* :: */[
              headCurrentRow,
              restCurrentRow
            ]);
        var potentialMaxAspectRatio = maxRowAspectRatio(sumValuesInTree(headRemaing), potentialRowValue, shortSide);
        if (potentialMaxAspectRatio < currentMaxAspectRatio) {
          _currentRow = List.concat(/* :: */[
                /* :: */[
                  headCurrentRow,
                  restCurrentRow
                ],
                /* :: */[
                  /* :: */[
                    headRemaing,
                    /* [] */0
                  ],
                  /* [] */0
                ]
              ]);
          _remaining = restRemaining;
          continue ;
        }
        var currentRowHaveValues = List.map((function (node) {
                return /* tuple */[
                        node,
                        sumValuesInTree(node)
                      ];
              }), /* :: */[
              headCurrentRow,
              restCurrentRow
            ]);
        var match = fitRowInContainer(currentRowHaveValues, container);
        _result = List.concat(/* :: */[
              match[0],
              /* :: */[
                result,
                /* [] */0
              ]
            ]);
        _container = match[1];
        _currentRow = /* [] */0;
        _remaining = /* :: */[
          headRemaing,
          restRemaining
        ];
        continue ;
      }
      _currentRow = /* :: */[
        headRemaing,
        /* [] */0
      ];
      _remaining = restRemaining;
      continue ;
    }
    if (!currentRow) {
      return result;
    }
    var currentRowHaveValues$1 = List.map((function (node) {
            return /* tuple */[
                    node,
                    sumValuesInTree(node)
                  ];
          }), /* :: */[
          currentRow[0],
          currentRow[1]
        ]);
    var match$1 = fitRowInContainer(currentRowHaveValues$1, container);
    _result = List.concat(/* :: */[
          match$1[0],
          /* :: */[
            result,
            /* [] */0
          ]
        ]);
    _container = match$1[1];
    _currentRow = /* [] */0;
    _remaining = /* [] */0;
    continue ;
  };
}

function traverse(input, container) {
  if (input.tag) {
    var nodesWithLayout = squarify(input[0], /* [] */0, container, /* [] */0);
    return List.concat(List.map((function (param) {
                      return traverse(param[0], param[1]);
                    }), nodesWithLayout));
  }
  var match = input[0];
  return /* :: */[
          {
            value: match.value,
            label: match.label,
            layout: container
          },
          /* [] */0
        ];
}

var testData = /* Branch */Block.__(1, [/* :: */[
      /* Leaf */Block.__(0, [{
            value: 6.0,
            label: "Container 0"
          }]),
      /* :: */[
        /* Branch */Block.__(1, [/* :: */[
              /* Branch */Block.__(1, [/* :: */[
                    /* Leaf */Block.__(0, [{
                          value: 3.0,
                          label: "Container 1.1.1"
                        }]),
                    /* :: */[
                      /* Leaf */Block.__(0, [{
                            value: 1.0,
                            label: "Container 1.1.2"
                          }]),
                      /* [] */0
                    ]
                  ]]),
              /* :: */[
                /* Leaf */Block.__(0, [{
                      value: 2.0,
                      label: "Container 1.2"
                    }]),
                /* [] */0
              ]
            ]]),
        /* :: */[
          /* Leaf */Block.__(0, [{
                value: 4.0,
                label: "Container 2"
              }]),
          /* :: */[
            /* Leaf */Block.__(0, [{
                  value: 3.0,
                  label: "Container 3"
                }]),
            /* :: */[
              /* Leaf */Block.__(0, [{
                    value: 2.0,
                    label: "Container 4"
                  }]),
              /* :: */[
                /* Leaf */Block.__(0, [{
                      value: 2.0,
                      label: "Container 5"
                    }]),
                /* :: */[
                  /* Leaf */Block.__(0, [{
                        value: 1.0,
                        label: "Container 6"
                      }]),
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ]
    ]]);

var testContainer = {
  x0: 0.0,
  y0: 0.0,
  x1: 6.0,
  y1: 4.0
};

var out = $$Array.of_list(traverse(testData, testContainer));

((console.dir(out,{depth: null})));

exports.getShorterSide = getShorterSide;
exports.sum = sum;
exports.sumValuesInTree = sumValuesInTree;
exports.maxOfList = maxOfList;
exports.minOfList = minOfList;
exports.maxRowAspectRatio = maxRowAspectRatio;
exports.fitRowInContainer = fitRowInContainer;
exports.squarify = squarify;
exports.traverse = traverse;
exports.testData = testData;
exports.testContainer = testContainer;
exports.out = out;
/* out Not a pure module */
