import React, { Component } from 'react';

import AppBar from 'material-ui/AppBar';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import TextField from 'material-ui/TextField';

import './App.css';

class InferenceForm extends Component {
  render() {
    return (
      <div className="inference-form">
        <h2>Input</h2>
        <TextField
          id="input"
          defaultValue="fun x -> x;;"
          multiLine={true}
          rows={2}
          rowsMax={4}
        />
        <h2>Output</h2>
        <TextField
          id="output"
          multiLine={true}
          rows={2}
          rowsMax={4}
        />
      </div>
    );
  }
}
class App extends Component {
  render() {
    return (
      <MuiThemeProvider>
        <div>
          <AppBar title="gsrinfer" />
          <InferenceForm />
        </div>
      </MuiThemeProvider>
    );
  }
}

export default App;
