{% extends "base.tpl" %}

{% block title %}{_ Run a Test Transaction _}{% endblock %}

{% block content %}

    <h1>{_ Run a Test Transaction Using Button _}</h1>

    {% button text="Pay €10"  postback={make_payment amount="1000"} delegate="mod_paybox" %}

    <h1>{_ Run a Test Transaction Using a Wire _}</h1>
    {% wire id="myform" type="submit" postback={make_payment amount="2000"} %}
    <form id="myform" method="post" action="postback" delegate="mod_paybox" postback="make_payment">
      <button id="mybutton" type="submit">Pay €20</button>
    </form>

{% endblock %}

