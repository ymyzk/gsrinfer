import React, { Component } from 'react';

import AppBar from 'material-ui/AppBar';
import DropDownMenu from 'material-ui/DropDownMenu';
import MenuItem from 'material-ui/MenuItem';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import { green500, orange500 } from 'material-ui/styles/colors';
import TextField from 'material-ui/TextField';

import './App.css';

const samples = [
  "fun x -> x",
  "(fun (x:?) -> x) 3",
  "reset (1 + 2)",
  "reset (3 + shift k -> true)",
  "reset (fun b -> shift k -> ((fun (x:?) -> x) 3))",
  "reset (fun b -> shift k -> if b then 123 else k ())",
  "fun b -> shift (k: ?) -> if b then (123 :: ?) else k ()",
  "reset ((shift k -> k) (shift l -> 123))",
];

class InferenceForm extends Component {
  constructor(props) {
    super(props);

    this.defaultInput = "fun x -> x";
  }

  handleChange = (event, value) => {
    const result = GsrInfer.infer(value);  // eslint-disable-line
    this.setState({
      program: value,
      result,
    });
  };

  handleSampleChange = (event, index, value) => {
    if (value == null) return;
    this.setState({
      program: value,
    });
    this.handleChange(event, value);
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
        Input a program here
        <TextField
          id="input"
          multiLine
          fullWidth
          rows={1}
          rowsMax={4}
          onChange={this.handleChange}
          value={this.state.program}
        />
        <DropDownMenu
          value={null}
          onChange={this.handleSampleChange}
          style={{width: "100%"}}
         >
          <MenuItem value={null} primaryText="Or, choose sample..." />
          {
            samples.map((s) => <MenuItem key={s} value={s} primaryText={s} />)
          }
        </DropDownMenu>
        <h2>Output</h2>
        <TextField
          id="output"
          multiLine
          fullWidth
          rows={1}
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
