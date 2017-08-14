# stylefmt.el
An emacs interface for [stylefmt](https://github.com/morishitter/stylefmt), gofmt inspired style code formatter.

# Installation

Install stylefmt

```
npm install -g stylefmt
```

then

```
M-x stylefmt-enable-on-save
```

in css editing buffer.

Optinally add to your init.el

```lisp
(add-hook 'css-mode-hook 'stylefmt-enable-on-save)
```

# Example


```style
      @media screen and (    min-width :699px)
 {.foo    +       .bar,.hoge{
    font-size   :12px      !   important   ;  ~       .fuga     {
      padding      : 10px       5px;
   color:green;
 >p

 {
        line-height             : 1.5          ;
      }}}
     }


.class,           #id
 {     color       : blue;

  border        :solid  #ddd                1px}
```

yields

```style
@media screen and (min-width: 699px) {
  .foo + .bar,
  .hoge {
    font-size: 12px !important;

    ~ .fuga {
      padding: 10px 5px;
      color: green;

      > p {
        line-height: 1.5;
      }
    }
  }
}


.class,
#id {
  color: blue;
  border: 1px solid #ddd;
}

```

