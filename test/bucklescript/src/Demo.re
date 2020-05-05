module FormattedMessage = {
  [@react.component] [@bs.module "react-intl"]
  external make:
    (
      ~id: option(string)=?,
      ~defaultMessage: string,
      ~values: Js.t({..})=?,
      ~tagName: string=?,
      ~children: (~formattedMessage: React.element) => React.element=?
    ) =>
    React.element =
    "FormattedMessage";
};

[@react.component]
let make = () => {
  <FormattedMessage defaultMessage="SomeDefaultMessage" />;
};