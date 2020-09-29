open ReactIntl;

module Msg = {
  [@intl.messages];
  let hello = {defaultMessage: "Hello"};
  let helloAgain = {defaultMessage: "Hello again", id: "AA"};
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
    <FormattedMessage defaultMessage="Some default message with id" id="BB" />;
  };
};
