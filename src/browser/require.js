function require(lib) {
    if (lib.slice(0,2) === './') {
        return window[lib.slice(2)];
    }
}
