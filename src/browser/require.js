/**
 * @license
 * Copyright 2018,2019 Shawn Betts
 * SPDX-License-Identifier: MIT
**/

function require(lib) {
    if (lib.slice(0,2) === './') {
        return window[lib.slice(2)];
    }
}
