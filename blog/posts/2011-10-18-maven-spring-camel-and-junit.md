---
title: Maven, spring, camel and JUnit
status: published
tags:
- java
- spring
- maven
---

I won't explain everything here because it's quite huge, but I just solved a nasty bug which took me many hours.

I want to be able to inject some spring/camel context during a JUnit test but I had the following errors :
<pre class="brush:java">Unexpected exception parsing XML document from class path resource [...] nested exception is org.springframework.beans.FatalBeanException: Invalid NamespaceHandler class [org.apache.camel.spring.handler.CamelNamespaceHandler] for namespace [http://camel.apache.org/schema/spring]: problem with handler class file or dependent class; nested exception is java.lang.NoSuchMethodError: org.slf4j.impl.StaticLoggerBinder.getSingleton()Lorg/slf4j/impl/StaticLoggerBinder;</pre>
I spent quite some time to figure out what was wrong with the namespaces, but everything was fine. The problem comes from slf4j. To solve it I had to update the maven dependencies so I only have <strong>one</strong> slf4j library in the classpath (be sure to use the latest one, 1.6.2 in my case). The bug happened just after a change of camel version, which probably triggered the download of a new version of slf4j, conflicting with the previous. In Eclipse with the maven plugin, right click&gt;update project configuration on the target project and it's fine.