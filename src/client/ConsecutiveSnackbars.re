/* WIP */

type action =
  | Message(string);

type messageInfo = {
  message: string,
  key: float,
};

type state = {
  _open: bool,
  msgInfo: messageInfo,
  messages: list(messageInfo),
};

let appendMsg = (messages, msg) => messages @ [{message: msg, key: Js.Date.now()}];

let component = ReasonReact.reducerComponent("ConsecutiveSnackbars");

let make = (_children, ~message) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Message(msg) => ReasonReact.Update({...state, _open: true, messages: appendMsg(state.messages, msg)})
    },
  initialState: () => {
    _open: false,
    msgInfo: {
      message: "",
      key: 0.0,
    },
    messages: [],
  },
  willReceiveProps: self => {
    Js.log("I got something: " ++ message);
    let newMessages = appendMsg(self.state.messages, message);
    let newMessage = {message, key: Js.Date.now()};
    {_open: true, msgInfo: newMessage, messages: newMessages};
  },
  render: ({state, send}) => {
    let msgEl = <span> (ReasonReact.string(state.msgInfo.message)) </span>;
    <MaterialUi.Snackbar key=(string_of_float(state.msgInfo.key)) open_=state._open message=msgEl />;
  },
};
