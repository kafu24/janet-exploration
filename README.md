# janet-exploration
Exploring Janet for the first time by implementing the VVQS5 language.

The concrete syntax of VVQS5 can be captured with the following EBNF:

<dl>
    <dt><i style="color:#5C739B">Expr</i> = <i style="color:#5C739B">Num</i></dt>
    <dd>| <i style="color:#5C739B">id</i></dd>
    <dd>| <i style="color:#5C739B">String</i></dd>
    <dd>| <t style="color:#D8B424">{</t><i style="color:#5C739B">Expr</i> if <i style="color:#5C739B">Expr</i> else <i style="color:#5C739B">Expr</i><t style="color:#D8B424">}</t></dd>
    <dd>| <t style="color:#D8B424">{</t><i style="color:#5C739B">Expr</i> where <t style="color:#D8B424">{</t><t style="color:#D8B424">[</t><i style="color:#5C739B">id := Expr</i><t style="color:#D8B424">]</t> <t style="color:maroon"> ...</t><t style="color:#D8B424">}</t><t style="color:#D8B424">}</t></dd>
    <dd>| <t style="color:#D8B424">{</t><t style="color:#D8B424">{</t><i style="color:#5C739B">id</i> <t style="color:maroon"> ...</t><t style="color:#D8B424">}</t> => <i style="color:#5C739B">Expr</i><t style="color:#D8B424">}</t></dd>
    <dd>| <t style="color:#D8B424">{</t><i style="color:#5C739B"> Expr Expr </i> <t style="color:maroon"> ...</t><t style="color:#D8B424">}</t>
</dl>


