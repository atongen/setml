type action =
  | Submit
  | Change(string);

type state = {name: string};

let component = ReasonReact.reducerComponent("NameDialog");

[@bs.deriving jsConverter]
type inputProps = {maxLength: int};

let make = (_children, ~open_, ~onCloseState, ~onCloseEvt, ~currentName, ~sendMessage) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Submit =>
      switch (String.trim(state.name)) {
      | "" => ReasonReact.NoUpdate
      | nonEmptyValue =>
        ReasonReact.UpdateWithSideEffects(
          {name: nonEmptyValue},
          (
            _self => {
              sendMessage(ClientUtil.make_name_msg(nonEmptyValue));
              onCloseState();
            }
          ),
        )
      }
    | Change(text) =>
      if (open_) {
        ReasonReact.Update({name: text});
      } else {
        ReasonReact.NoUpdate;
      }
    },
  initialState: () => {name: currentName},
  render: self => {
    let submitClick = _evt => self.send(Submit);
    let handleChange = evt => self.send(Change(ReactEvent.Form.target(evt)##value));
    MaterialUi.(
      <Dialog open_ onClose=onCloseEvt>
        <DialogTitle>
          (ReasonReact.string("Set Your Name"))
          <IconButton key="close" color=`Inherit onClick=(_evt => onCloseState())>
            <Icon> (ReasonReact.string("close")) </Icon>
          </IconButton>
        </DialogTitle>
        <form noValidate=false autoComplete="off">
          <DialogContent>
            <TextField
              defaultValue=(`String(currentName))
              onChange=handleChange
              label=(ReasonReact.string("Name"))
              required=true
              inputProps=(inputPropsToJs({maxLength: 16}))
              fullWidth=true
              autoFocus=true
            />
          </DialogContent>
          <DialogActions>
            <Button color=`Primary onClick=(_evt => onCloseState())> (ReasonReact.string("Cancel")) </Button>
            <Button color=`Primary onClick=submitClick> (ReasonReact.string("Ok")) </Button>
          </DialogActions>
        </form>
      </Dialog>
    );
  },
};
