---
title: Webservice from contract with tomcat
tags: java, tomcat, webservice
status: published
---

<p>I just spent quite some time to create a prototype to tests some stuff about web service security. But for that, I first needed to set up another environment.</p>
<p>this is going to be a <strong>long</strong> post, so brace yourself... Let's start with a quick overview of the project.</p>
<h1>Overview</h1>
<p><strong>goal</strong> : setup a web application running with tomcat to provide a simple web service. It is going to be a simple greeter.</p>
<ul>
<li>Create the "contract" of the web service, <em>ie</em> the wsdl file.</li>
<li>Generate a skeleton of java code to build the web service and implement it.</li>
<li>Configure the web application</li>
<li>Deploy and test</li>
</ul>
<p>&nbsp;</p>
<h1>Create the contract</h1>
<p>The webservice is going to be name greeter and the wsdl file greeter.wsdl. I used <a href="http://wiki.eclipse.org/index.php/Introduction_to_the_WSDL_Editor">wsdl editor</a>, an eclipse plugin to do that. It is quite intuitive to create a simple web service using the graphical interface. Watch out for the service definition (usually towards the end of the file) :</p>
<pre class="brush:xml">  &lt;wsdl:service name="Greeter"&gt;
    &lt;wsdl:port binding="tns:GreeterSOAP" name="GreeterSOAP"&gt;
      &lt;soap:address location="http://localhost:8080/&lt;CONTEXT&gt;/&lt;SERVLET_ENDPOINT&gt;/&lt;WSNAME&gt;"/&gt;
    &lt;/wsdl:port&gt;
  &lt;/wsdl:service&gt;</pre>
<p>where &lt;CONTEXT&gt; is going to be the name of your web application, &lt;SERVLET_ENDPOINT&gt; is going to be used to configure the mapping between URL and java classes, and &lt;WSNAME&gt; is the name name of your webservice.</p>
<p>&nbsp;</p>
<h1>Generate the java files</h1>
<p>Running maven with the cxf plugin should produce an interface called Greeter.java. Any implementation of this class (I used GreeterImpl.java) will do. Some details about the pom file needed to compile :</p>
<pre class="brush:xml">    &lt;properties&gt;
        &lt;camel.version&gt;2.8.1&lt;/camel.version&gt;
        &lt;spring.version&gt;3.0.6.RELEASE&lt;/spring.version&gt;
        &lt;cxf.version&gt;2.4.2&lt;/cxf.version&gt;
    &lt;/properties&gt;

         &lt;build&gt;
		&lt;plugins&gt;
			&lt;plugin&gt;
				&lt;artifactId&gt;maven-compiler-plugin&lt;/artifactId&gt;
				&lt;version&gt;2.3.2&lt;/version&gt;
				&lt;configuration&gt;&lt;/configuration&gt;
			&lt;/plugin&gt;
			&lt;plugin&gt;
				&lt;artifactId&gt;maven-eclipse-plugin&lt;/artifactId&gt;
				&lt;version&gt;2.8&lt;/version&gt;
				&lt;configuration&gt;&lt;/configuration&gt;
			&lt;/plugin&gt;

			&lt;!-- Generate code based on the WSDL --&gt;
			&lt;plugin&gt;
				&lt;groupId&gt;org.apache.cxf&lt;/groupId&gt;
				&lt;artifactId&gt;cxf-codegen-plugin&lt;/artifactId&gt;
				&lt;version&gt;${cxf.version}&lt;/version&gt;
				&lt;executions&gt;
					&lt;execution&gt;
						&lt;id&gt;generate-sources&lt;/id&gt;
						&lt;phase&gt;generate-sources&lt;/phase&gt;
						&lt;configuration&gt;
							&lt;sourceRoot&gt;${basedir}/src/main/java&lt;/sourceRoot&gt;
							&lt;wsdlRoot&gt;${basedir}/src/main/resources/wsdl&lt;/wsdlRoot&gt;
							&lt;includes&gt;
								&lt;include&gt;*.wsdl&lt;/include&gt;
							&lt;/includes&gt;

						&lt;/configuration&gt;
						&lt;goals&gt;
							&lt;goal&gt;wsdl2java&lt;/goal&gt;
						&lt;/goals&gt;
					&lt;/execution&gt;

				&lt;/executions&gt;
				&lt;dependencies&gt;
					&lt;dependency&gt;
						&lt;groupId&gt;org.springframework&lt;/groupId&gt;
						&lt;artifactId&gt;spring-core&lt;/artifactId&gt;
						&lt;version&gt;${spring.version}&lt;/version&gt;
					&lt;/dependency&gt;
				&lt;/dependencies&gt;
			&lt;/plugin&gt;

			&lt;!-- This plugin is configured in order to scan resources in the webapp
				folder in order to substitute profile properties --&gt;

		&lt;/plugins&gt;
	&lt;/build&gt;</pre>
<p>Eclipse and maven plugin to compile. the cxf plugin generate the java code from the wsdl (here in java/main/resources/wsdl).</p>
<p>&nbsp;</p>
<h1>Configure the web application</h1>
<p>Here it can be tricky... First, we're going to need another maven plugin to generate the .war :</p>
<pre class="brush:xml">			&lt;plugin&gt;
				&lt;groupId&gt;org.apache.maven.plugins&lt;/groupId&gt;
				&lt;artifactId&gt;maven-war-plugin&lt;/artifactId&gt;
				&lt;version&gt;2.0.2&lt;/version&gt;
				&lt;configuration&gt;
					&lt;webResources&gt;
						&lt;resource&gt;
							&lt;filtering&gt;true&lt;/filtering&gt;
							&lt;directory&gt;${project.basedir}/src/main/webapp/&lt;/directory&gt;
							&lt;includes&gt;
								&lt;include&gt;**/*.xml&lt;/include&gt;
							&lt;/includes&gt;
						&lt;/resource&gt;
					&lt;/webResources&gt;
				&lt;/configuration&gt;
			&lt;/plugin&gt;</pre>
<p>You saw that the plugin is looking for every xml files in the directory /src/main/webapp/**. So we're going to put the web.xml file there (precisely in /src/main/webapp/META-INF/). The headers have nothing special :</p>
<pre class="brush:xml">&lt;?xml version="1.0" encoding="UTF-8"?&gt;
&lt;web-app version="2.5" metadata-complete="true"
xmlns="http://java.sun.com/xml/ns/javaee" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd"&gt;</pre>
<p>Then, the cxf plugin is using spring so we need to declare a spring context configuration file along with a listener for this context :</p>
<pre class="brush:xml">	&lt;context-param&gt;
		&lt;param-name&gt;contextConfigLocation&lt;/param-name&gt;
		&lt;param-value&gt;/WEB-INF/spring-ws.xml&lt;/param-value&gt;
	&lt;/context-param&gt;
	&lt;listener&gt;
		&lt;listener-class&gt;org.springframework.web.context.ContextLoaderListener&lt;/listener-class&gt;
	&lt;/listener&gt;</pre>
<p>And then, add the cxf servlet. At this point, you have to remember the variable you put in your wsdl file, the &lt;SERVLET_ENDPOINT&gt;.</p>
<pre class="brush:xml">    &lt;servlet&gt;
        &lt;servlet-name&gt;CXFServlet&lt;/servlet-name&gt;
        &lt;display-name&gt;CXF Servlet&lt;/display-name&gt;
        &lt;servlet-class&gt;org.apache.cxf.transport.servlet.CXFServlet&lt;/servlet-class&gt;
    &lt;/servlet&gt;
    &lt;servlet-mapping&gt;
        &lt;servlet-name&gt;CXFServlet&lt;/servlet-name&gt;
        &lt;url-pattern&gt;/&lt;SERVLET_ENDPOINT&gt;/*&lt;/url-pattern&gt;
    &lt;/servlet-mapping&gt;</pre>
<p>So everything with the url localhost:8080/&lt;CONTEXT&gt;/&lt;SERVLET_ENDPOINT&gt;/whatever is going to be handle by the cxf servlet. Now let's write thespring-ws.xml file to configure spring and the endpoints.</p>
<pre class="brush:xml">&lt;beans xmlns="http://www.springframework.org/schema/beans"
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xmlns:jaxws="http://cxf.apache.org/jaxws"
xmlns:jaxrs="http://cxf.apache.org/jaxrs"
xmlns:context="http://www.springframework.org/schema/context"

xsi:schemaLocation="
http://www.springframework.org/schema/beans
http://www.springframework.org/schema/beans/spring-beans-2.0.xsd
http://cxf.apache.org/jaxws
http://cxf.apache.org/schemas/jaxws.xsd
http://cxf.apache.org/jaxrs
http://cxf.apache.org/schemas/jaxrs.xsd
http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context.xsd
http://www.springframework.org/schema/tx http://www.springframework.org/schema/tx/spring-tx-2.0.xsd"&gt;

	&lt;context:annotation-config /&gt;
	&lt;import resource="classpath:META-INF/cxf/cxf.xml" /&gt;

	&lt;jaxws:endpoint id="greeterWS"
	implementor="&lt;WEB_SERVICE_CLASS&gt;" address="/greeter" /&gt;
&lt;/beans&gt;</pre>
<p>What does that do ? First, import a resource (which is actually inside a .jar imported by maven, see the final pom file at the end) and then, declare an endpoint for the webservice. the variable &lt;WEB_SERVICE_CLASS&gt; should be the fully qualified name of your class without the .java extension. This class is the one implementing the interface Greeter.java, in my case it's still GreeterImpl.java</p>
<p>Here are the dependencies for the cxf plugin :</p>
<pre class="brush:xml">	&lt;dependencies&gt;

		&lt;!-- CXF --&gt;
		&lt;dependency&gt;
			&lt;groupId&gt;org.apache.cxf&lt;/groupId&gt;
			&lt;artifactId&gt;cxf-rt-core&lt;/artifactId&gt;
			&lt;version&gt;${cxf.version}&lt;/version&gt;
		&lt;/dependency&gt;

		&lt;dependency&gt;
			&lt;groupId&gt;org.apache.cxf&lt;/groupId&gt;
			&lt;artifactId&gt;cxf-rt-frontend-jaxws&lt;/artifactId&gt;
			&lt;version&gt;${cxf.version}&lt;/version&gt;
		&lt;/dependency&gt;
		&lt;dependency&gt;
			&lt;groupId&gt;org.apache.cxf&lt;/groupId&gt;
			&lt;artifactId&gt;cxf-rt-transports-http&lt;/artifactId&gt;
			&lt;version&gt;${cxf.version}&lt;/version&gt;
		&lt;/dependency&gt;

	&lt;/dependencies&gt;</pre>
<h1>Deploy and test</h1>
<p>And here we are, finally. Run the goal install with maven. (don't forget to put the packaging to war)</p>
<pre class="brush:xml">&lt;packaging&gt;war&lt;/packaging&gt;</pre>
<p>Then, copy the resulting war in your apache-tomcat/webapp folder and restart it. It should automatically create a folder with the same name as the .war. The URL localhost:8080/&lt;CONTEXT&gt;/&lt;SERVLET_ENDPOINT&gt;/greeter should display an xml message (error message probably if there is an expected input). You can try the webservice using SoapUI.</p>
<p>&nbsp;</p>
<p>And it's done. The main pitfall is the dependencies of the cxf plugin in the pomfile.</p>