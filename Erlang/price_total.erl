-module(price_total).
-export([summary/0]).

summary() -> Cart = [ {apple, 3, 0.5}, {orange, 2, 0.75}, {banana, 6, 0.25} ],
             [ {Item, Quantity * Price} || {Item, Quantity, Price} <- Cart ].