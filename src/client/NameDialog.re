type action =
  | Submit
  | Change(string);

type state = {name: string};

let component = ReasonReact.reducerComponent("NameDialog");

let make = (_children, ~open_, ~onClose, ~currentName, ~sendMessage) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Submit =>
      switch (String.trim(state.name)) {
        | "" => ReasonReact.NoUpdate
        | nonEmptyValue =>
          ReasonReact.UpdateWithSideEffects(
            {name: nonEmptyValue},
            (_self => sendMessage(ClientUtil.make_name_msg(nonEmptyValue))),
          )
        };
    | Change(text) =>
        if (open_) {
            ReasonReact.Update({name: text})
        } else {
            ReasonReact.NoUpdate
        };
    },
  initialState: () => {name: currentName},
  render: self => {
    let submitClick = _evt => self.send(Submit);
    let handleChange = evt => self.send(Change(ReactEvent.Form.target(evt)##value));
    <MaterialUi.Dialog open_ onClose>
      <MaterialUi.DialogTitle> (ReasonReact.string("Set Your Name")) </MaterialUi.DialogTitle>
      <div>
        <MaterialUi.TextField defaultValue=(`String(currentName)) onChange=handleChange />
        <MaterialUi.Button onClick=submitClick variant=`Contained> (ReasonReact.string("Submit")) </MaterialUi.Button>
      </div>
    </MaterialUi.Dialog>;
  },
};
