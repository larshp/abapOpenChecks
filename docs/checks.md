---
title: Checks
---

{% assign aChecks = site.checks | sort: "index" %}
{% for check in aChecks %}
<a href="{{ check.url }}">
{{ check.cNumber }} - {{ check.title }}
</a>
{% if check.rfc %}<img src="/img/rfc.png" title="RFC-based">{% endif %}
{% endfor %}