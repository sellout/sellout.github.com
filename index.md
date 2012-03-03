---
layout: page
title: Wandering through the technical landscape
tagline: and rapidly running out of metaphorical water
---
{% include JB/setup %}

I am a Lisp hacker and programming language enthusiast. Hopefully this blog serves as more than just a place for me to refer back to, but we'll see.
    
## Posts

<dl class="posts">
  {% for post in site.posts %}
    <dt><a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></dt>
    <dd><span>{{ post.date | date_to_string }}</span></dd>
    <dd><span>{{ post.content | truncatewords: 50 }}</span></dd>
  {% endfor %}
</dl>
