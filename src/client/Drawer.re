let component = ReasonReact.statelessComponent("Drawer");

[%mui.withStyles "StyledDrawer"({root: ReactDOMRe.Style.make(~width="250px", ())})];

let make = (_children, ~isOpen, ~onClose) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <StyledDrawer
        render=(
          classes =>
            <Drawer variant=`Temporary open_=isOpen onClose>
              <div className=classes.root>
                 <div>
                   <ListItem>
                     <ListItemIcon> <Icon> (ReasonReact.string("inbox")) </Icon> </ListItemIcon>
                     <ListItemText primary=(ReasonReact.string("Inbox")) />
                   </ListItem>
                   <ListItem>
                     <ListItemIcon> <Icon> (ReasonReact.string("star")) </Icon> </ListItemIcon>
                     <ListItemText primary=(ReasonReact.string("Starred")) />
                   </ListItem>
                   <ListItem>
                     <ListItemIcon> <Icon> (ReasonReact.string("send")) </Icon> </ListItemIcon>
                     <ListItemText primary=(ReasonReact.string("Send mail")) />
                   </ListItem>
                   <ListItem>
                     <ListItemIcon> <Icon> (ReasonReact.string("drafts")) </Icon> </ListItemIcon>
                     <ListItemText primary=(ReasonReact.string("Drafts")) />
                   </ListItem>
                 </div>
                 <Divider />
                 <div>
                   <ListItem>
                     <ListItemIcon> <Icon> (ReasonReact.string("mail")) </Icon> </ListItemIcon>
                     <ListItemText primary=(ReasonReact.string("All mail")) />
                   </ListItem>
                   <ListItem>
                     <ListItemIcon> <Icon> (ReasonReact.string("delete")) </Icon> </ListItemIcon>
                     <ListItemText primary=(ReasonReact.string("Trash")) />
                   </ListItem>
                   <ListItem>
                     <ListItemIcon> <Icon> (ReasonReact.string("report")) </Icon> </ListItemIcon>
                     <ListItemText primary=(ReasonReact.string("Spam")) />
                   </ListItem>
                 </div>
              </div>
            </Drawer>
        )
      />
    ),
};
