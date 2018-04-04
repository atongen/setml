open Belt;

module Game = {
    type action =
    | Message(string);
    type state = {messages: list(string)};
    let component = ReasonReact.reducerComponent("Game");
    let make = _children  => {
        ...component,
        reducer: (action, state) =>
            switch (action) {
            | Message(message) =>
                let messages = state.messages @ [message];
                ReasonReact.Update({...state, messages})
            },
        initialState: () => {
            messages: ["wowza!"]
        },
        render: ({state, send}) => {
            let messages = state.messages |> List.map(_, message => <li>(ReasonReact.stringToElement(message))</li>);
            <section className="main">
                <button onClick=(_event => send(Message("Did it.")))>
                    (ReasonReact.stringToElement("Do it."))
                </button>
                <ul className="messages">
                    (ReasonReact.arrayToElement(List.toArray(messages)))
                </ul>
            </section>;
        }
    };
};

ReactDOMRe.renderToElementWithId(<Game />, "game");
