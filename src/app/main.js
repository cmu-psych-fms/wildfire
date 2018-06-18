const url = require('url');
const path = require('path');
const defaultMenu = require('electron-default-menu');
const electron = require('electron');

// Module to control application life.
const app = electron.app;
// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow;

let mainWindow;

function createWindow() {
    mainWindow = new BrowserWindow({width: 1024, height: 768});
    mainWindow.loadURL(url.format({pathname: path.join(__dirname, 'index.html'),
                                   protocol: 'file:',
                                   slashes: true}));
    mainWindow.on('closed', function () {
        mainWindow = null;
    });
    const menu = defaultMenu(app, electron.shell);
    electron.Menu.setApplicationMenu(electron.Menu.buildFromTemplate(menu));
    // Just in case it's unstable.
    mainWindow.openDevTools();
    mainWindow.maximize();
}

app.on('ready', createWindow);
app.on('window-all-closed', function () {
    app.quit();
});

