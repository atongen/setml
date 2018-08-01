open Belt;

/* https://github.com/mui-org/material-ui/blob/master/docs/src/pages/demos/snackbars/ConsecutiveSnackbars.js */


type action =
  | Message(string);

type messageInfo = {
  message: option(string),
  key: float,
};

type state = {
  _open: bool,
  msgInfo: option(messageInfo),
  messages: list(messageInfo)
};

let appendMsg = (messages, msg) => messages @ [{message: msg, key: Js.Date.now()}];

let component = ReasonReact.reducerComponent("ConsecutiveSnackbars");

let make = (_children, ~message) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Message(msg) => ReasonReact.Update({...state, open_: true, messages: appendMsg(state.messages, msg)})
    },
  initialState: () => {
    open_: false,
    msgInfo: None,
    messages: ref(Belt.MutableQueue.make()),
  },
  willReceiveProps: self => {
      switch(message) {
      | Some(msg) =>

      | None => self.state
      }
    Js.log("I got something: " ++ message);
    let newMessages = appendMsg(self.state.messages, message);
    let newMessage = {message, key: Js.Date.now()};
    {_open: true, msgInfo: newMessage, messages: newMessages};
  },
  render: ({state, send}) => {
    let msgEl = <span> (ReasonReact.string(state.msgInfo.message)) </span>;
    <MaterialUi.Snackbar key=(string_of_float(state.msgInfo.key)) open_=state.open_ message=msgEl />;
  },
};
