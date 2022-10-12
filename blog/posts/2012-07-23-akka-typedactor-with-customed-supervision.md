---
title: Akka - typedActor with customed supervision
status: published
tags:
- akka
---

Recently I have been asigned some R&amp;D work about <a href="http://akka.io/">akka</a>. For those who don't know what it is, basically it is an api (built on scala with a java version) to enable people to create asynchronous application without bothering with locking problems. This is achieved using the actor model.

Akka provide what is called typed actor as en interface between the akka world and the outer world. These actor are created from an interface and a class which implemets this interface. I wanted to create a typed actor with a precise strategy in case of exception, and I had some problem with that. Here is some snippets of code to achieve that:

```
public class ServiceSupervisor extends UntypedActor {

  private static SupervisorStrategy strategy = new OneForOneStrategy(5, Duration.parse("1 minute"), new Function<Throwable, Directive>() {
  @Override
  public Directive apply(Throwable t) {

    if(t==null)
    throw new NullPointerException();

    if (t instanceof RuntimeException) {
      logger.error("received a RuntimeException, ignored and resume the actor.";
      return SupervisorStrategy.resume();
    } else
      return stop();
    }
  });

  @Override
  public SupervisorStrategy supervisorStrategy() {
    return strategy;
  }

  public void onReceive(Object o) {

  logger.debug("supervisor received message: " + o);
  if (o instanceof Props) {
    //Create a new supervised untyped actor
    ActorRef newChild = getContext().actorOf((Props) o);
    getSender().tell(newChild);
  } else if (o instanceof TypedProps<>) {
    //creating a typedActor
    getSender().tell(TypedActor.get(getContext()).typedActorOf((TypedProps&lt;?&gt;) o));
  } else {
    unhandled(o);
  }
}
```
```
  ActorRef supervisor = system.actorOf(new Props(ServiceSupervisor.class),&quot;supervisor&quot;);

  TypedProps tp = new TypedProps(TypedInterface.class, TypedClass.class);

  TypedInterface supervised = (TypedInterface) Await.result(Patterns.ask(supervisor, tp, 5000),Duration.parse("5 seconds"));
```

The supervisor resume the actor if it encounters a RuntimeException, and stop the actor if it is something else. A supervisor is automatically set when one creates the actor, it cannot be set afterwards. So the onReceive method allow one to get actors references created with the context of the supervisor.
After that, it is possible to put the actors in a router or do whatever you want, they are going to use the supervision strategy defined above.

For instance, create a router:

```
TypedProps tp = new TypedProps(TypedInterface.class,TypedClass.class);
List routees = new LinkedList();
for (int i = 0; i < 1; ++i)
  routees.add((TypedInterface) Await.result(Patterns.ask(supervisor, tp, 5000),Duration.parse("5 seconds")));

//create the router and apply the custom strategy
ActorRef router = system.actorOf(Props().withRouter(RoundRobinRouter.create(routees)));
```

Happy hakking