open ReactIntl;

module Msg = {
  [@intl.messages];
  let hello = {defaultMessage: "Hello"};
  let helloAgain = {defaultMessage: "Hello again", id: "A"};
};

module Component1 = {
  [@react.component]
  let make = () => {
    <FormattedMessage defaultMessage="Some default message" />;
  };
};

module Component2 = {
  [@react.component]
  let make = () => {
    <FormattedMessage defaultMessage="Some default message with id" id="B" />;
  };
};

module Component3 = {
  [@react.component]
  let make = () => {
    <FormattedMessage
      defaultMessage="Some default message with id and values {foo}"
      id="C"
      values={
        "foo":
          <FormattedMessage defaultMessage="Somme wrapped message" id="D" />,
      }
    />;
  };
};
