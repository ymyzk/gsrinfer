import React, { Component } from 'react';

import AppBar from 'material-ui/AppBar';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import { green500, orange500 } from 'material-ui/styles/colors';
import TextField from 'material-ui/TextField';

import './App.css';

class InferenceForm extends Component {
  constructor(props) {
    super(props);

    this.defaultInput = "fun x -> x;;";
    this.state = {
      result: {
        isSucceeded: true,
        result: "",
      },
    };
  }

  handleChange = (event, newValue) => {
    const result = GsrInfer.infer(newValue);  // eslint-disable-line
    this.setState({
      result,
    });
  };

  componentWillMount() {
    this.handleChange(null, this.defaultInput);
  };

  render() {
    const message = this.state.result.result;
    const underlineStyle = {
      borderColor: this.state.result.isSucceeded ? green500 : orange500,
    };
    return (
      <div className="inference-form">
        <h2>Input</h2>
        <TextField
          id="input"
          defaultValue={this.defaultInput}
          multiLine
          fullWidth
          rows={2}
          rowsMax={4}
          onChange={this.handleChange}
        />
        <h2>Output</h2>
        <TextField
          id="output"
          multiLine
          fullWidth
          rows={2}
          rowsMax={4}
          value={message}
          underlineStyle={underlineStyle}
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
