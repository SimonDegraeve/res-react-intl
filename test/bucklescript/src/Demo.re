open ReactIntl;

module Msg = {
  [@intl.messages];
  let hello = {defaultMessage: "Hello"};
};

[@react.component]
let make = () => {
  <FormattedMessage defaultMessage="Some default message" />;
};
