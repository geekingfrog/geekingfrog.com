---
title: Object pooling in Spring
status: published
tags:
- java
- spring
---

I spent a lot of time trying to activate a special feature of object pooling in spring, and now I finally found a workaround I'll post that.

First some words about object pooling. The idea here is to have a pool of objects (webservice clients connected to something) available for other method to use without having to instantiate them. Because there are some specials headers, these objects are not stateless, and thus a singleton for every other method is not acceptable. Hence the object pool. Below the basic configuration for an object pool (in your spring configuration xml file)

```
<bean id="targetBean" class="my.compagny.TargetBean" scope="prototype">
</bean>

<bean id="poolTargetBean" class="org.springframework.aop.target.CommonsPoolTargetSource">
  <property name="targetBeanName" value="targetBean" />
  <property name="maxSize" value="10"/>
  <property name="maxIdle" value="10" />
  <property name="minIdle" value="5" />
</bean>
```
And then in your code when you need the targetBean :

```
@Autowired
CommonsPoolTargetSource pool;
...
TargetBean bean = (TargetBean) pool.getTarget();
(do some stuff with the bean)
pool.releaseTarget(bean);
```

This works fine with one caveat : the minIdle property do nothing. I would expect the pool to create 5 TargetBean when I start the application.
However, no bean is created. In my case, the creation of the targetBean is costly and I cannot afford to create them at runtime when needed (at least not all of them). The solution
(after a lof of googling and testing) was to create a custom pool, which extends the spring's CommonsPoolTargetSource and add an init-method.
This is probably not the best solution but that's a satisfactory workaround.

new spring config for the pool :

```
<bean id="pooledSeInfoClient" class="my.compagny.MyPool" init-method="initMyPool">
  <property name="targetBeanName" value="targetBean" />
  <property name="maxSize" value="10"/>
  <property name="maxIdle" value="10" />
  <property name="minIdle" value="5" />
</bean>
```

The code for the custom pool :

```
public class MyPool extends CommonsPoolTargetSource {

  public void initMyPool() throws Exception {
    List<Object> beans = new ArrayList<Object>();

    //create and retain minIdle objects to force the creation of others
    for(int i=0 ; i<this.getMinIdle() ; ++i)
    beans.add(this.getTarget());

    for(Object o : beans)
      this.releaseTarget(o);
  }
}
```

And in the code, just replace the reference to CommonsPoolTargetSource by the custom pool.