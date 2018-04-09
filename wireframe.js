function Wireframe(points, runs, color) {
    Object.call(this);
    this.points = points;
    this.runs = runs;
    this.color = color;
    return this;
}

Wireframe.prototype = {};

// Wireframe.prototype.rotate_translate = function (pair, x, y, angle) {
//     return [ pair[0] * Math.cos(angle) - pair[1] * Math.sin(angle) + x,
//             -pair[1] * Math.sin(angle) + pair[1] * Math.cos(angle) + y];
// }

// Wireframe.prototype.translate_run = function (run, x, y, angle) {
//     var i, new_run = [];
//     for (i=0; i<run.length; i++) {
//         new_run[i] = this.rotate_translate(run[i][0]);
//     }
// };

Wireframe.prototype.draw = function (canvas, x, y, angle) {
    var r, i;
    canvas.save();
    canvas.strokeStyle = this.color;
    canvas.translate(x,y);
    canvas.rotate(angle * Math.PI / 180);
    for (r=0; r<this.runs.length; r++) {
        canvas.beginPath();
        canvas.moveTo(this.points[this.runs[r][0]][0], this.points[this.runs[r][0]][1]);
        for (i=1; i<this.runs[r].length; i++) {
            canvas.lineTo(this.points[this.runs[r][i]][0], this.points[this.runs[r][i]][1]);
        }
        canvas.stroke();
    }
    canvas.restore();
};

var missileWireframe = new Wireframe([[0, 0], [-25, 0], [-5, 5], [-5, -5]],
                                     [[0,1], [0, 2], [0, 3]],
                                    '#FFFFFF');
var shellWireframe = new Wireframe([[-8, 0], [0, -6], [16, 0], [0, 6], [-8, 0]],
                                   [[0,1,2,3,0]],
                                   '#FF0000');
var shipWireframe = new Wireframe([[-18,0], [18,0], [0,0], [-18,18], [-18,-18]],
                                  [[0,1], [3, 2, 4]],
                                  '#FFFF00');
var fortressWireframe = new Wireframe([[0, 0], [36, 0], [18, -18], [0,-18], [18,18], [0,18]],
                                      [[0, 1], [3,2,4,5]],
                                     '#FFFF00');
