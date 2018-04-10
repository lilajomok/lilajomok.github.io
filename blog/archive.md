---
title: Archive
layout: page
---

<ul>
{% for post in site.posts %}
  <li>
    <strong>{{ post.date | date: '%-B %-d'}}</strong> &raquo; <a href="{{ post.url }}">{{ post.title }}</a>
  </li> 
{% endfor %}
</ul>

