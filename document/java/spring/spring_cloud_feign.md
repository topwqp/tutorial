



## OpenFeign调用流程

启动流程： 

![image-20220225165935040](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220225165935040.png)

### EnableFeignClients

### 

```java
/**
 * Scans for interfaces that declare they are feign clients (via
 * {@link org.springframework.cloud.openfeign.FeignClient} <code>@FeignClient</code>).
 * Configures component scanning directives for use with
 * {@link org.springframework.context.annotation.Configuration}
 * <code>@Configuration</code> classes.
 *
 * @author Spencer Gibb
 * @author Dave Syer
 * @since 1.0
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Documented
@Import(FeignClientsRegistrar.class)
public @interface EnableFeignClients {

	/**
	 * Alias for the {@link #basePackages()} attribute. Allows for more concise annotation
	 * declarations e.g.: {@code @ComponentScan("org.my.pkg")} instead of
	 * {@code @ComponentScan(basePackages="org.my.pkg")}.
	 * @return the array of 'basePackages'.
	 */
	String[] value() default {};

	/**
	 * Base packages to scan for annotated components.
	 * <p>
	 * {@link #value()} is an alias for (and mutually exclusive with) this attribute.
	 * <p>
	 * Use {@link #basePackageClasses()} for a type-safe alternative to String-based
	 * package names.
	 * @return the array of 'basePackages'.
	 */
	String[] basePackages() default {};

	/**
	 * Type-safe alternative to {@link #basePackages()} for specifying the packages to
	 * scan for annotated components. The package of each class specified will be scanned.
	 * <p>
	 * Consider creating a special no-op marker class or interface in each package that
	 * serves no purpose other than being referenced by this attribute.
	 * @return the array of 'basePackageClasses'.
	 */
	Class<?>[] basePackageClasses() default {};

	/**
	 * A custom <code>@Configuration</code> for all feign clients. Can contain override
	 * <code>@Bean</code> definition for the pieces that make up the client, for instance
	 * {@link feign.codec.Decoder}, {@link feign.codec.Encoder}, {@link feign.Contract}.
	 *
	 * @see FeignClientsConfiguration for the defaults
	 * @return list of default configurations
	 */
	Class<?>[] defaultConfiguration() default {};

	/**
	 * List of classes annotated with @FeignClient. If not empty, disables classpath
	 * scanning.
	 * @return list of FeignClient classes
	 */
	Class<?>[] clients() default {};

}

```



### FeignClientsRegistrar

feignClientsRegistrar 注册处理`org.springframework.cloud.openfeign.FeignClientsRegistrar`

**整体流程就是扫描com.xs.micro.consumer**

启动流程中作用就是把@FeignClient对应的注解扫描到，并生成BeanDefinition

![image-20220225172912772](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220225172912772.png)

```java
@Override
	public void registerBeanDefinitions(AnnotationMetadata metadata,
			BeanDefinitionRegistry registry) {
		registerDefaultConfiguration(metadata, registry);
		registerFeignClients(metadata, registry);
	}

private void registerDefaultConfiguration(AnnotationMetadata metadata,
			BeanDefinitionRegistry registry) {
		Map<String, Object> defaultAttrs = metadata
				.getAnnotationAttributes(EnableFeignClients.class.getName(), true);

		if (defaultAttrs != null && defaultAttrs.containsKey("defaultConfiguration")) {
			String name;
			if (metadata.hasEnclosingClass()) {
				name = "default." + metadata.getEnclosingClassName();
			}
			else {
				name = "default." + metadata.getClassName();
			}
			registerClientConfiguration(registry, name,
					defaultAttrs.get("defaultConfiguration"));
		}
	}

	public void registerFeignClients(AnnotationMetadata metadata,
			BeanDefinitionRegistry registry) {
		ClassPathScanningCandidateComponentProvider scanner = getScanner();
		scanner.setResourceLoader(this.resourceLoader);

		Set<String> basePackages;

		Map<String, Object> attrs = metadata
				.getAnnotationAttributes(EnableFeignClients.class.getName());
		AnnotationTypeFilter annotationTypeFilter = new AnnotationTypeFilter(
				FeignClient.class);
		final Class<?>[] clients = attrs == null ? null
				: (Class<?>[]) attrs.get("clients");
		if (clients == null || clients.length == 0) {
			scanner.addIncludeFilter(annotationTypeFilter);
			basePackages = getBasePackages(metadata);
		}
		else {
			final Set<String> clientClasses = new HashSet<>();
			basePackages = new HashSet<>();
			for (Class<?> clazz : clients) {
				basePackages.add(ClassUtils.getPackageName(clazz));
				clientClasses.add(clazz.getCanonicalName());
			}
			AbstractClassTestingTypeFilter filter = new AbstractClassTestingTypeFilter() {
				@Override
				protected boolean match(ClassMetadata metadata) {
					String cleaned = metadata.getClassName().replaceAll("\\$", ".");
					return clientClasses.contains(cleaned);
				}
			};
			scanner.addIncludeFilter(
					new AllTypeFilter(Arrays.asList(filter, annotationTypeFilter)));
		}

		for (String basePackage : basePackages) {
			Set<BeanDefinition> candidateComponents = scanner
					.findCandidateComponents(basePackage);
			for (BeanDefinition candidateComponent : candidateComponents) {
				if (candidateComponent instanceof AnnotatedBeanDefinition) {
					// verify annotated class is an interface
					AnnotatedBeanDefinition beanDefinition = (AnnotatedBeanDefinition) candidateComponent;
					AnnotationMetadata annotationMetadata = beanDefinition.getMetadata();
					Assert.isTrue(annotationMetadata.isInterface(),
							"@FeignClient can only be specified on an interface");

					Map<String, Object> attributes = annotationMetadata
							.getAnnotationAttributes(
									FeignClient.class.getCanonicalName());

					String name = getClientName(attributes);
					registerClientConfiguration(registry, name,
							attributes.get("configuration"));

					registerFeignClient(registry, annotationMetadata, attributes);
				}
			}
		}
	}

	private void registerFeignClient(BeanDefinitionRegistry registry,
			AnnotationMetadata annotationMetadata, Map<String, Object> attributes) {
		String className = annotationMetadata.getClassName();
		BeanDefinitionBuilder definition = BeanDefinitionBuilder
				.genericBeanDefinition(FeignClientFactoryBean.class);
		validate(attributes);
		definition.addPropertyValue("url", getUrl(attributes));
		definition.addPropertyValue("path", getPath(attributes));
		String name = getName(attributes);
		definition.addPropertyValue("name", name);
		String contextId = getContextId(attributes);
		definition.addPropertyValue("contextId", contextId);
		definition.addPropertyValue("type", className);
		definition.addPropertyValue("decode404", attributes.get("decode404"));
		definition.addPropertyValue("fallback", attributes.get("fallback"));
		definition.addPropertyValue("fallbackFactory", attributes.get("fallbackFactory"));
		definition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);

		String alias = contextId + "FeignClient";
		AbstractBeanDefinition beanDefinition = definition.getBeanDefinition();
		beanDefinition.setAttribute(FactoryBean.OBJECT_TYPE_ATTRIBUTE, className);

		// has a default, won't be null
		boolean primary = (Boolean) attributes.get("primary");

		beanDefinition.setPrimary(primary);

		String qualifier = getQualifier(attributes);
		if (StringUtils.hasText(qualifier)) {
			alias = qualifier;
		}

		BeanDefinitionHolder holder = new BeanDefinitionHolder(beanDefinition, className,
				new String[] { alias });
		BeanDefinitionReaderUtils.registerBeanDefinition(holder, registry);
	}

	private void validate(Map<String, Object> attributes) {
		AnnotationAttributes annotation = AnnotationAttributes.fromMap(attributes);
		// This blows up if an aliased property is overspecified
		// FIXME annotation.getAliasedString("name", FeignClient.class, null);
		validateFallback(annotation.getClass("fallback"));
		validateFallbackFactory(annotation.getClass("fallbackFactory"));
	}

	/* for testing */ String getName(Map<String, Object> attributes) {
		String name = (String) attributes.get("serviceId");
		if (!StringUtils.hasText(name)) {
			name = (String) attributes.get("name");
		}
		if (!StringUtils.hasText(name)) {
			name = (String) attributes.get("value");
		}
		name = resolve(name);
		return getName(name);
	}

	private String getContextId(Map<String, Object> attributes) {
		String contextId = (String) attributes.get("contextId");
		if (!StringUtils.hasText(contextId)) {
			return getName(attributes);
		}

		contextId = resolve(contextId);
		return getName(contextId);
	}

	private String resolve(String value) {
		if (StringUtils.hasText(value)) {
			return this.environment.resolvePlaceholders(value);
		}
		return value;
	}

	private String getUrl(Map<String, Object> attributes) {
		String url = resolve((String) attributes.get("url"));
		return getUrl(url);
	}

	private String getPath(Map<String, Object> attributes) {
		String path = resolve((String) attributes.get("path"));
		return getPath(path);
	}

	protected ClassPathScanningCandidateComponentProvider getScanner() {
		return new ClassPathScanningCandidateComponentProvider(false, this.environment) {
			@Override
			protected boolean isCandidateComponent(
					AnnotatedBeanDefinition beanDefinition) {
				boolean isCandidate = false;
				if (beanDefinition.getMetadata().isIndependent()) {
					if (!beanDefinition.getMetadata().isAnnotation()) {
						isCandidate = true;
					}
				}
				return isCandidate;
			}
		};
	}

	protected Set<String> getBasePackages(AnnotationMetadata importingClassMetadata) {
		Map<String, Object> attributes = importingClassMetadata
				.getAnnotationAttributes(EnableFeignClients.class.getCanonicalName());

		Set<String> basePackages = new HashSet<>();
		for (String pkg : (String[]) attributes.get("value")) {
			if (StringUtils.hasText(pkg)) {
				basePackages.add(pkg);
			}
		}
		for (String pkg : (String[]) attributes.get("basePackages")) {
			if (StringUtils.hasText(pkg)) {
				basePackages.add(pkg);
			}
		}
		for (Class<?> clazz : (Class[]) attributes.get("basePackageClasses")) {
			basePackages.add(ClassUtils.getPackageName(clazz));
		}

		if (basePackages.isEmpty()) {
			basePackages.add(
					ClassUtils.getPackageName(importingClassMetadata.getClassName()));
		}
		return basePackages;
	}

	private String getQualifier(Map<String, Object> client) {
		if (client == null) {
			return null;
		}
		String qualifier = (String) client.get("qualifier");
		if (StringUtils.hasText(qualifier)) {
			return qualifier;
		}
		return null;
	}

	private String getClientName(Map<String, Object> client) {
		if (client == null) {
			return null;
		}
		String value = (String) client.get("contextId");
		if (!StringUtils.hasText(value)) {
			value = (String) client.get("value");
		}
		if (!StringUtils.hasText(value)) {
			value = (String) client.get("name");
		}
		if (!StringUtils.hasText(value)) {
			value = (String) client.get("serviceId");
		}
		if (StringUtils.hasText(value)) {
			return value;
		}

		throw new IllegalStateException("Either 'name' or 'value' must be provided in @"
				+ FeignClient.class.getSimpleName());
	}

	private void registerClientConfiguration(BeanDefinitionRegistry registry, Object name,
			Object configuration) {
		BeanDefinitionBuilder builder = BeanDefinitionBuilder
				.genericBeanDefinition(FeignClientSpecification.class);
		builder.addConstructorArgValue(name);
		builder.addConstructorArgValue(configuration);
		registry.registerBeanDefinition(
				name + "." + FeignClientSpecification.class.getSimpleName(),
				builder.getBeanDefinition());
	}

	@Override
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}

	/**
	 * Helper class to create a {@link TypeFilter} that matches if all the delegates
	 * match.
	 *
	 * @author Oliver Gierke
	 */
	private static class AllTypeFilter implements TypeFilter {

		private final List<TypeFilter> delegates;

		/**
		 * Creates a new {@link AllTypeFilter} to match if all the given delegates match.
		 * @param delegates must not be {@literal null}.
		 */
		AllTypeFilter(List<TypeFilter> delegates) {
			Assert.notNull(delegates, "This argument is required, it must not be null");
			this.delegates = delegates;
		}

		@Override
		public boolean match(MetadataReader metadataReader,
				MetadataReaderFactory metadataReaderFactory) throws IOException {

			for (TypeFilter filter : this.delegates) {
				if (!filter.match(metadataReader, metadataReaderFactory)) {
					return false;
				}
			}

			return true;
		}

	}
```

Root bean: class [org.springframework.boot.autoconfigure.SharedMetadataReaderFactoryContextInitializer$SharedMetadataReaderFactoryBean]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null

#### org.springframework.cloud.openfeign.FeignClientFactoryBean

注意： 因为feign初始化本身比较复杂，是采用自定义FactoryBean进行初始化的，我们之前跟踪过spring的源码，spring源码中在进行

初始化时，普通类初始化采用的是构造方法反射获取对象，还有一种是实现了`FactoryBean`接口的，采用的是另外一种方式初始化，具体代码参考：  `org.springframework.beans.factory.support.DefaultListableBeanFactory#preInstantiateSingletons`

```java
@Override
	public void preInstantiateSingletons() throws BeansException {
		if (logger.isTraceEnabled()) {
			logger.trace("Pre-instantiating singletons in " + this);
		}

		// Iterate over a copy to allow for init methods which in turn register new bean definitions.
		// While this may not be part of the regular factory bootstrap, it does otherwise work fine.
		List<String> beanNames = new ArrayList<>(this.beanDefinitionNames);

		// Trigger initialization of all non-lazy singleton beans...
		for (String beanName : beanNames) {
			RootBeanDefinition bd = getMergedLocalBeanDefinition(beanName);
			if (!bd.isAbstract() && bd.isSingleton() && !bd.isLazyInit()) {
				if (isFactoryBean(beanName)) {
					Object bean = getBean(FACTORY_BEAN_PREFIX + beanName);
					if (bean instanceof FactoryBean) {
						FactoryBean<?> factory = (FactoryBean<?>) bean;
						boolean isEagerInit;
						if (System.getSecurityManager() != null && factory instanceof SmartFactoryBean) {
							isEagerInit = AccessController.doPrivileged(
									(PrivilegedAction<Boolean>) ((SmartFactoryBean<?>) factory)::isEagerInit,
									getAccessControlContext());
						}
						else {
							isEagerInit = (factory instanceof SmartFactoryBean &&
									((SmartFactoryBean<?>) factory).isEagerInit());
						}
						if (isEagerInit) {
							getBean(beanName);
						}
					}
				}
				else {
					getBean(beanName);
				}
			}
		}

		// Trigger post-initialization callback for all applicable beans...
		for (String beanName : beanNames) {
			Object singletonInstance = getSingleton(beanName);
			if (singletonInstance instanceof SmartInitializingSingleton) {
				SmartInitializingSingleton smartSingleton = (SmartInitializingSingleton) singletonInstance;
				if (System.getSecurityManager() != null) {
					AccessController.doPrivileged((PrivilegedAction<Object>) () -> {
						smartSingleton.afterSingletonsInstantiated();
						return null;
					}, getAccessControlContext());
				}
				else {
					smartSingleton.afterSingletonsInstantiated();
				}
			}
		}
	}


```



#### FeignClient对象创建过程

核心在于 `org.springframework.cloud.openfeign.FeignClientFactoryBean#getObject`

```java
	@Override
	public Object getObject() throws Exception {
		return getTarget();
	}

	/**
	 * @param <T> the target type of the Feign client
	 * @return a {@link Feign} client created with the specified data and the context
	 * information
	 */
	<T> T getTarget() {
		FeignContext context = this.applicationContext.getBean(FeignContext.class);
		Feign.Builder builder = feign(context);

		if (!StringUtils.hasText(this.url)) {
			if (!this.name.startsWith("http")) {
				this.url = "http://" + this.name;
			}
			else {
				this.url = this.name;
			}
			this.url += cleanPath();
			return (T) loadBalance(builder, context,
					new HardCodedTarget<>(this.type, this.name, this.url));
		}
		if (StringUtils.hasText(this.url) && !this.url.startsWith("http")) {
			this.url = "http://" + this.url;
		}
		String url = this.url + cleanPath();
		Client client = getOptional(context, Client.class);
		if (client != null) {
			if (client instanceof LoadBalancerFeignClient) {
				// not load balancing because we have a url,
				// but ribbon is on the classpath, so unwrap
				client = ((LoadBalancerFeignClient) client).getDelegate();
			}
			if (client instanceof FeignBlockingLoadBalancerClient) {
				// not load balancing because we have a url,
				// but Spring Cloud LoadBalancer is on the classpath, so unwrap
				client = ((FeignBlockingLoadBalancerClient) client).getDelegate();
			}
			builder.client(client);
		}
		Targeter targeter = get(context, Targeter.class);
		return (T) targeter.target(this, builder, context,
				new HardCodedTarget<>(this.type, this.name, url));
	}

```



![image-20220225180524175](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220225180524175.png)



继续往下调用

![image-20220225181244522](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220225181244522.png)



#### FeignClientFactoryBean初始化



getObject()调用getTarget()

![image-20220225182038062](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220225182038062.png)



初始化的过程中，会把feign对应的编解码器初始化

![image-20220225181833234](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220225181833234.png)



#### org.springframework.cloud.openfeign.HystrixTargeter





```java
class HystrixTargeter implements Targeter {

	@Override
	public <T> T target(FeignClientFactoryBean factory, Feign.Builder feign,
			FeignContext context, Target.HardCodedTarget<T> target) {
    //如果没有开启hystrix熔断配置，直接返回feign.target，没有引入和配置hystrix走这里
		if (!(feign instanceof feign.hystrix.HystrixFeign.Builder)) {
			return feign.target(target);
		}
    //开始hystrix调用，这块后期分析
		feign.hystrix.HystrixFeign.Builder builder = (feign.hystrix.HystrixFeign.Builder) feign;
		String name = StringUtils.isEmpty(factory.getContextId()) ? factory.getName()
				: factory.getContextId();
		SetterFactory setterFactory = getOptional(name, context, SetterFactory.class);
		if (setterFactory != null) {
			builder.setterFactory(setterFactory);
		}
		Class<?> fallback = factory.getFallback();
		if (fallback != void.class) {
			return targetWithFallback(name, context, target, builder, fallback);
		}
		Class<?> fallbackFactory = factory.getFallbackFactory();
		if (fallbackFactory != void.class) {
			return targetWithFallbackFactory(name, context, target, builder,
					fallbackFactory);
		}

		return feign.target(target);
	}
```



#### 最后真正来到这里： feign.Feign.Builder#target(feign.Target<T>)

```java
public <T> T target(Target<T> target) {
      //build()
      return build().newInstance(target);
    }

    public Feign build() {
      Client client = Capability.enrich(this.client, capabilities);
      Retryer retryer = Capability.enrich(this.retryer, capabilities);
      List<RequestInterceptor> requestInterceptors = this.requestInterceptors.stream()
          .map(ri -> Capability.enrich(ri, capabilities))
          .collect(Collectors.toList());
      Logger logger = Capability.enrich(this.logger, capabilities);
      Contract contract = Capability.enrich(this.contract, capabilities);
      Options options = Capability.enrich(this.options, capabilities);
      Encoder encoder = Capability.enrich(this.encoder, capabilities);
      Decoder decoder = Capability.enrich(this.decoder, capabilities);
      InvocationHandlerFactory invocationHandlerFactory =
          Capability.enrich(this.invocationHandlerFactory, capabilities);
      QueryMapEncoder queryMapEncoder = Capability.enrich(this.queryMapEncoder, capabilities);

      SynchronousMethodHandler.Factory synchronousMethodHandlerFactory =
          new SynchronousMethodHandler.Factory(client, retryer, requestInterceptors, logger,
              logLevel, decode404, closeAfterDecode, propagationPolicy, forceDecoding);
      ParseHandlersByName handlersByName =
          new ParseHandlersByName(contract, options, encoder, decoder, queryMapEncoder,
              errorDecoder, synchronousMethodHandlerFactory);
      return new ReflectiveFeign(handlersByName, invocationHandlerFactory, queryMapEncoder);
    }
  }
```



#### 真正创建FeignClient的代理对象feign.ReflectiveFeign#newInstance

```java
/**
   * creates an api binding to the {@code target}. As this invokes reflection, care should be taken
   * to cache the result.
   */
  @SuppressWarnings("unchecked")
  @Override
  public <T> T newInstance(Target<T> target) {
    Map<String, MethodHandler> nameToHandler = targetToHandlersByName.apply(target);
    Map<Method, MethodHandler> methodToHandler = new LinkedHashMap<Method, MethodHandler>();
    List<DefaultMethodHandler> defaultMethodHandlers = new LinkedList<DefaultMethodHandler>();

    for (Method method : target.type().getMethods()) {
      if (method.getDeclaringClass() == Object.class) {
        continue;
      } else if (Util.isDefault(method)) {
        DefaultMethodHandler handler = new DefaultMethodHandler(method);
        defaultMethodHandlers.add(handler);
        methodToHandler.put(method, handler);
      } else {
        methodToHandler.put(method, nameToHandler.get(Feign.configKey(target.type(), method)));
      }
    }
    InvocationHandler handler = factory.create(target, methodToHandler);
    T proxy = (T) Proxy.newProxyInstance(target.type().getClassLoader(),
        new Class<?>[] {target.type()}, handler);

    for (DefaultMethodHandler defaultMethodHandler : defaultMethodHandlers) {
      defaultMethodHandler.bindTo(proxy);
    }
    return proxy;
  }
```

如下，创建代理对象成功： 

这种直接通过`T proxy = (T) Proxy.newProxyInstance(target.type().getClassLoader(),
    new Class<?>[] {target.type()}, handler);` 方式创建的代理对象也纳入了spring的管理



![image-20220225182930707](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220225182930707.png)



#### 代理对象核心逻辑

`InvocationHandler`动态代理的实现`FeignInvocationHandler`

```java
static class FeignInvocationHandler implements InvocationHandler {

    private final Target target;
    private final Map<Method, MethodHandler> dispatch;

    FeignInvocationHandler(Target target, Map<Method, MethodHandler> dispatch) {
      this.target = checkNotNull(target, "target");
      this.dispatch = checkNotNull(dispatch, "dispatch for %s", target);
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
      if ("equals".equals(method.getName())) {
        try {
          Object otherHandler =
              args.length > 0 && args[0] != null ? Proxy.getInvocationHandler(args[0]) : null;
          return equals(otherHandler);
        } catch (IllegalArgumentException e) {
          return false;
        }
      } else if ("hashCode".equals(method.getName())) {
        return hashCode();
      } else if ("toString".equals(method.getName())) {
        return toString();
      }
      //调用不同的方法，执行不同的方法调用
      return dispatch.get(method).invoke(args);
    }

```

#### 核心逻辑调试

![image-20220225183403164](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220225183403164.png)



#### feign调用

![image-20220227110656277](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220227110656277.png)

以上是feign的动态代理创建@FeignClient对应的接口的实现类，代理后核心实现逻辑是： `feign.ReflectiveFeign.FeignInvocationHandler#invoke`

核心实现：  `return dispatch.get(method).invoke(args);`

dispatch定义如下： 

```java
    private final Map<Method, MethodHandler> dispatch;

```



`dispatch.get(method)`获取的是对应的MethodHandler

#### MethodHandler

`feign.InvocationHandlerFactory.MethodHandler` 

这个实现类有哪些？？

![image-20220227110806077](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220227110806077.png)



真正调用是如下方法

`feign.SynchronousMethodHandler#invoke`

​      `feign.ReflectiveFeign.BuildTemplateByResolvingArgs#create`

​            `feign.RequestTemplate#from`

​                     `feign.RequestTemplate`

​                             `feign.SynchronousMethodHandler#executeAndDecode`

​                                     `org.springframework.cloud.openfeign.ribbon.LoadBalancerFeignClient#execute` 

​                                             `org.springframework.cloud.openfeign.ribbon.LoadBalancerFeignClient#getClientConfig`

​                                                     `org.springframework.cloud.netflix.ribbon.SpringClientFactory#getClientConfig`

​                                                            `org.springframework.cloud.netflix.ribbon.SpringClientFactory#getInstance`

-----> 接上

 `org.springframework.cloud.openfeign.ribbon.CachingSpringLoadBalancerFactory#create`

​           ``



#### 逻辑关系

整体上大的逻辑关系是： `FeignClientFactoryBean`是一个`factoryBean`，这个bean在进行生成对应的BeanDefinition的过程，

这个 `FeignClientFactoryBean ` 初始化的过程中，会进行 `FeignContext`的初始化，对应的FeignContext中有个属性值 `private Map<String, AnnotationConfigApplicationContext> contexts = new ConcurrentHashMap<>();`  这里面存储了多个子容器`AnnotationConfigApplicationContext`，这个每个子context，都是一个 `@FeignClient`中的contextId属性信息





####    feignContext和原容器的父子关系

上下文相关的断点类： `org.springframework.cloud.context.named.NamedContextFactory`

注意： `contextId` 每一个定义，都会新建一个 `AnnotationConfigApplicationContext`，和原启动的`org.springframework.boot.web.servlet.context.AnnotationConfigServletWebApplicationContext` 是父子容器的关系，



`FeignContext`继承关系如下： 

```java
public class FeignContext extends NamedContextFactory<FeignClientSpecification> {


public abstract class NamedContextFactory<C extends NamedContextFactory.Specification>
		implements DisposableBean, ApplicationContextAware {
  	private final String propertySourceName;

	private final String propertyName;

  //这里的contexts中，每一个AnnotationConfigApplicationContext 就是@FeignClient中对应的contextId属性信息
	private Map<String, AnnotationConfigApplicationContext> contexts = new ConcurrentHashMap<>();

	private Map<String, C> configurations = new ConcurrentHashMap<>();

	private ApplicationContext parent;

	private Class<?> defaultConfigType;
```





```java
package com.xs.micro.consumer.domain.partner;

import com.xs.micro.consumer.domain.intercepter.FeignInterceptor;
import com.xs.micro.consumer.domain.pojo.dto.UserDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * @author wqp
 * @desc feign invoke
 * @date 2022年02月09日16:01:35
 */
@FeignClient(name = "micro-provider", contextId = "provider-client" ,fallback = InvokeFeignClientFallback.class,  configuration = FeignInterceptor.class)
public interface InvokeFeignClient {

    /**
     * test app header
     * @param userDTO
     * @return
     */
    @PostMapping(value = "/user/http/header")
    String testHttpHeader(@RequestBody UserDTO userDTO);



    /**
     * @return
     */
    @GetMapping(value = "/provider/ping")
    String ping();

}

```



![image-20220228111420153](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228111420153.png)

具体显示父子容器的位置：  `org.springframework.cloud.context.named.NamedContextFactory#getInstance(java.lang.String, java.lang.Class<T>)`





![image-20220228105622221](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228105622221.png)



再一次显示父子容器关系

![image-20220228105851291](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228105851291.png)



子容器FeignContext中多个`AnnotationConfigApplicationContext` 多个 `@FeignClient`多个`contextId`  是多个 `AnnotationConfigApplicationContext` 类型的上下文



这里明确显示了，子容器需要初始化的对象的`BeanDefinition` 的定义

![image-20220228110841850](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228110841850.png)





#### clientConfig

![image-20220228102707710](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228102707710.png)



#### feign.SynchronousMethodHandler#targetRequest

这里有一个扩展点，实现   `RequestInterceptor` 接口即可： 

```java
 Request targetRequest(RequestTemplate template) {
    for (RequestInterceptor interceptor : requestInterceptors) {
      interceptor.apply(template);
    }
    return target.apply(template);
  }
```



这个拦截器，可以自定义拦截器，

```java
/**
 * @author wqp
 * @desc feign interceptor
 * @date 2022年02月09日15:49:01
 */

@Slf4j
public class FeignInterceptor implements RequestInterceptor {
    private static   int count = 2;
    @Override
    public void apply(RequestTemplate requestTemplate) {
        log.info("feign interceptor invoke start ..........");
        requestTemplate.header("userId", "temp" + count++);

        log.info("feign interceptor invoke end ...........");
    }
}

```







![image-20220227142011748](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220227142011748.png)









### feignContext上下文初始化

#### FeignAutoConfiguration

`org.springframework.cloud.openfeign.FeignAutoConfiguration`

```java
@Bean
	public FeignContext feignContext() {
		FeignContext context = new FeignContext();
		context.setConfigurations(this.configurations);
		return context;
	}

```

这个feignContext如何和applicationContext绑定的呢？？？

看下父类的构造方法：

```java
public class FeignContext extends NamedContextFactory<FeignClientSpecification> {
     	public FeignContext() {
		super(FeignClientsConfiguration.class, "feign", "feign.client.name");
	}
  
父类： 
  public abstract class NamedContextFactory<C extends NamedContextFactory.Specification>
		implements DisposableBean, ApplicationContextAware {

    父类实现了 ApplicationContextAware
      
   所以feignContext从NamedContextFactory 中继承了 applicationContext，然后和自己绑定上了
```



OpenFeign调用处理：

feign.ReflectiveFeign

```

```



#### 启动初始化bean流程



org.springframework.beans.factory.support.AbstractBeanFactory#getBean(java.lang.String)

​          org.springframework.beans.factory.support.AbstractBeanFactory#getObjectForBeanInstance

​               org.springframework.beans.factory.support.FactoryBeanRegistrySupport#getObjectFromFactoryBean

​                    org.springframework.beans.factory.support.FactoryBeanRegistrySupport#doGetObjectFromFactoryBean

​                           org.springframework.cloud.openfeign.FeignClientFactoryBean#getObject

​                                       org.springframework.cloud.openfeign.FeignClientFactoryBean#getTarget



  接上------------>                                             

`Feign.Builder builder = feign(context);`

​    org.springframework.cloud.openfeign.FeignClientFactoryBean#feign

​                `Feign.Builder builder = get(context, Feign.Builder.class)`    

​                        org.springframework.cloud.openfeign.FeignClientFactoryBean#get

​                                   org.springframework.cloud.context.named.NamedContextFactory#getInstance(java.lang.String, java.lang.Class<T>)

​                                     org.springframework.cloud.context.named.NamedContextFactory#getContext

​                                                org.springframework.cloud.context.named.NamedContextFactory#createContext

上下文创建完成以后: 继续往下： `Feign.Builder builder = feign(context);`  核心处理不少，就是为了feignContext相关的对象

​               

接上： -------------->

      return (T) loadBalance(builder, context,
      new HardCodedTarget<>(this.type, this.name, this.url));

​     

​           



#### 创建FeignContext中的子容器的代码

```java
protected AnnotationConfigApplicationContext createContext(String name) {
		AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
		if (this.configurations.containsKey(name)) {
			for (Class<?> configuration : this.configurations.get(name)
					.getConfiguration()) {
				context.register(configuration);
			}
		}
		for (Map.Entry<String, C> entry : this.configurations.entrySet()) {
			if (entry.getKey().startsWith("default.")) {
				for (Class<?> configuration : entry.getValue().getConfiguration()) {
					context.register(configuration);
				}
			}
		}
		context.register(PropertyPlaceholderAutoConfiguration.class,
				this.defaultConfigType);
		context.getEnvironment().getPropertySources().addFirst(new MapPropertySource(
				this.propertySourceName,
				Collections.<String, Object>singletonMap(this.propertyName, name)));
		if (this.parent != null) {
			// Uses Environment from parent as well as beans
			context.setParent(this.parent);
			// jdk11 issue
			// https://github.com/spring-cloud/spring-cloud-netflix/issues/3101
			context.setClassLoader(this.parent.getClassLoader());
		}
		context.setDisplayName(generateDisplayName(name));
		context.refresh();
		return context;
	}
```



![image-20220228141931283](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228141931283.png)



#### 子容器的真正的初始化流程

![image-20220228142312911](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228142312911.png)



#### 继续factorybean核心创建对象分析

```java
/**
	 * @param <T> the target type of the Feign client
	 * @return a {@link Feign} client created with the specified data and the context
	 * information
	 */
	<T> T getTarget() {
		FeignContext context = this.applicationContext.getBean(FeignContext.class);
		Feign.Builder builder = feign(context);

		if (!StringUtils.hasText(this.url)) {
			if (!this.name.startsWith("http")) {
				this.url = "http://" + this.name;
			}
			else {
				this.url = this.name;
			}
			this.url += cleanPath();
			return (T) loadBalance(builder, context,
					new HardCodedTarget<>(this.type, this.name, this.url));
		}
		if (StringUtils.hasText(this.url) && !this.url.startsWith("http")) {
			this.url = "http://" + this.url;
		}
		String url = this.url + cleanPath();
		Client client = getOptional(context, Client.class);
		if (client != null) {
			if (client instanceof LoadBalancerFeignClient) {
				// not load balancing because we have a url,
				// but ribbon is on the classpath, so unwrap
				client = ((LoadBalancerFeignClient) client).getDelegate();
			}
			if (client instanceof FeignBlockingLoadBalancerClient) {
				// not load balancing because we have a url,
				// but Spring Cloud LoadBalancer is on the classpath, so unwrap
				client = ((FeignBlockingLoadBalancerClient) client).getDelegate();
			}
			builder.client(client);
		}
		Targeter targeter = get(context, Targeter.class);
		return (T) targeter.target(this, builder, context,
				new HardCodedTarget<>(this.type, this.name, url));
	}
```



#### loadBalance核心代码分析

```java
return (T) loadBalance(builder, context,
      new HardCodedTarget<>(this.type, this.name, this.url));
```

org.springframework.cloud.openfeign.FeignClientFactoryBean#loadBalance

​         feign.Target.HardCodedTarget

​             org.springframework.cloud.openfeign.FeignClientFactoryBean#getOptional

​                      org.springframework.cloud.context.named.NamedContextFactory#getInstance(java.lang.String, java.lang.Class<T>)

​                                feign.Client

​                                     org.springframework.cloud.openfeign.ribbon.DefaultFeignLoadBalancedConfiguration#feignClient

接上 ------->                  

​           feign.Feign.Builder#client               

​                   org.springframework.cloud.openfeign.HystrixTargeter#target

​                             feign.Feign.Builder#target(feign.Target<T>)

​                                   feign.Feign.Builder#build

​                                           feign.SynchronousMethodHandler.Factory#Factory                        

​                                                      ReflectiveFeign

`return new ReflectiveFeign(handlersByName, invocationHandlerFactory, queryMapEncoder);`

​    feign.ReflectiveFeign#newInstance

​              feign.ReflectiveFeign.BuildTemplateByResolvingArgs

​                   feign.SynchronousMethodHandler.Factory#create

​                         feign.SynchronousMethodHandler#SynchronousMethodHandler             

​                               feign.AsyncResponseHandler#AsyncResponseHandler            



`Map<String, MethodHandler> result = new LinkedHashMap<String, MethodHandler>();`



feign.InvocationHandlerFactory.Default#create

​       feign.ReflectiveFeign.FeignInvocationHandler#FeignInvocationHandler



最终创建的代理对象为：` feign.ReflectiveFeign#newInstance    `



![image-20220228152943324](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228152943324.png)           





#### 获取的result中 methodHandler内容

![image-20220228151242650](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228151242650.png)



`Map<Method, MethodHandler> methodToHandler = new LinkedHashMap<Method, MethodHandler>();`



#### new LinkedHashMap<Method, MethodHandler>

![image-20220228151609208](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228151609208.png)









#### Feign 和ribbon接口点

![image-20220228145430481](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228145430481.png)







#### HardCodedTarget

![image-20220228143443630](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228143443630.png)           



#### Feign.Client

![image-20220228143901151](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228143901151.png)



#### 实例化@Bean注解的工厂方法

`org.springframework.cloud.openfeign.ribbon.DefaultFeignLoadBalancedConfiguration#feignClient`

![image-20220228145153282](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228145153282.png)



------------

**以上是spring cloud项目启动时，初始化的bean对象，其中主要是对`FeignClientFactoryBean`的初始化**



### feign请求调用debug

开启  `return dispatch.get(method).invoke(args);`







​             

#### feign.SynchronousMethodHandler#invoke

```java
 @Override
  public Object invoke(Object[] argv) throws Throwable {
    RequestTemplate template = buildTemplateFromArgs.create(argv);
    Options options = findOptions(argv);
    Retryer retryer = this.retryer.clone();
    while (true) {
      try {
        return executeAndDecode(template, options);
      } catch (RetryableException e) {
        try {
          retryer.continueOrPropagate(e);
        } catch (RetryableException th) {
          Throwable cause = th.getCause();
          if (propagationPolicy == UNWRAP && cause != null) {
            throw cause;
          } else {
            throw th;
          }
        }
        if (logLevel != Logger.Level.NONE) {
          logger.logRetry(metadata.configKey(), logLevel);
        }
        continue;
      }
    }
  }
```



![image-20220228155333694](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228155333694.png)



####  feign.ReflectiveFeign.BuildTemplateByResolvingArgs#create

```java
@Override
    public RequestTemplate create(Object[] argv) {
      RequestTemplate mutable = RequestTemplate.from(metadata.template());
      mutable.feignTarget(target);
      if (metadata.urlIndex() != null) {
        int urlIndex = metadata.urlIndex();
        checkArgument(argv[urlIndex] != null, "URI parameter %s was null", urlIndex);
        mutable.target(String.valueOf(argv[urlIndex]));
      }
      Map<String, Object> varBuilder = new LinkedHashMap<String, Object>();
      for (Entry<Integer, Collection<String>> entry : metadata.indexToName().entrySet()) {
        int i = entry.getKey();
        Object value = argv[entry.getKey()];
        if (value != null) { // Null values are skipped.
          if (indexToExpander.containsKey(i)) {
            value = expandElements(indexToExpander.get(i), value);
          }
          for (String name : entry.getValue()) {
            varBuilder.put(name, value);
          }
        }
      }

      RequestTemplate template = resolve(argv, mutable, varBuilder);
      if (metadata.queryMapIndex() != null) {
        // add query map parameters after initial resolve so that they take
        // precedence over any predefined values
        Object value = argv[metadata.queryMapIndex()];
        Map<String, Object> queryMap = toQueryMap(value);
        template = addQueryMapQueryParameters(queryMap, template);
      }

      if (metadata.headerMapIndex() != null) {
        template =
            addHeaderMapHeaders((Map<String, Object>) argv[metadata.headerMapIndex()], template);
      }

      return template;
    }
```



#### feign.SynchronousMethodHandler#executeAndDecode

```java
Object executeAndDecode(RequestTemplate template, Options options) throws Throwable {
    //转换为request请求
    Request request = targetRequest(template);

    if (logLevel != Logger.Level.NONE) {
      logger.logRequest(metadata.configKey(), logLevel, request);
    }

    Response response;
    long start = System.nanoTime();
    try {
      //执行真正的调用逻辑
      response = client.execute(request, options);
      // ensure the request is set. TODO: remove in Feign 12
      response = response.toBuilder()
          .request(request)
          .requestTemplate(template)
          .build();
    } catch (IOException e) {
      if (logLevel != Logger.Level.NONE) {
        logger.logIOException(metadata.configKey(), logLevel, e, elapsedTime(start));
      }
      throw errorExecuting(request, e);
    }
    long elapsedTime = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);


    if (decoder != null)
      return decoder.decode(response, metadata.returnType());

    //异步响应处理
    CompletableFuture<Object> resultFuture = new CompletableFuture<>();
    asyncResponseHandler.handleResponse(resultFuture, metadata.configKey(), response,
        metadata.returnType(),
        elapsedTime);

    try {
      if (!resultFuture.isDone())
        throw new IllegalStateException("Response handling not done");

      return resultFuture.join();
    } catch (CompletionException e) {
      Throwable cause = e.getCause();
      if (cause != null)
        throw cause;
      throw e;
    }
  }
```



#### org.springframework.cloud.openfeign.ribbon.LoadBalancerFeignClient#execute

```java
@Override
	public Response execute(Request request, Request.Options options) throws IOException {
		try {
      //获取请求url
			URI asUri = URI.create(request.url());
			String clientName = asUri.getHost();
      //主机地址
			URI uriWithoutHost = cleanUrl(request.url(), clientName);
      //获取ribbonRequest
			FeignLoadBalancer.RibbonRequest ribbonRequest = new FeignLoadBalancer.RibbonRequest(
					this.delegate, request, uriWithoutHost);

      //获取对应的客户端配置
			IClientConfig requestConfig = getClientConfig(options, clientName);
			return lbClient(clientName)
					.executeWithLoadBalancer(ribbonRequest, requestConfig).toResponse();
		}
		catch (ClientException e) {
			IOException io = findIOException(e);
			if (io != null) {
				throw io;
			}
			throw new RuntimeException(e);
		}
	}

```



#### org.springframework.cloud.openfeign.ribbon.FeignLoadBalancer.RibbonRequest#RibbonRequest

```java
protected RibbonRequest(Client client, Request request, URI uri) {
			this.client = client;
			setUri(uri);
			this.request = toRequest(request);
		}

		private Request toRequest(Request request) {
			Map<String, Collection<String>> headers = new LinkedHashMap<>(
					request.headers());
			return Request.create(request.httpMethod(), getUri().toASCIIString(), headers,
					request.body(), request.charset(), request.requestTemplate());
		}


/**
   * Builds a Request. All parameters must be effectively immutable, via safe copies.
   *
   * @param httpMethod for the request.
   * @param url for the request.
   * @param headers to include.
   * @param body of the request, can be {@literal null}
   * @return a Request
   */
  public static Request create(HttpMethod httpMethod,
                               String url,
                               Map<String, Collection<String>> headers,
                               Body body,
                               RequestTemplate requestTemplate) {
    return new Request(httpMethod, url, headers, body, requestTemplate);
  }

  
```



#### 构建request

```java
private final HttpMethod httpMethod;
  private final String url;
  private final Map<String, Collection<String>> headers;
  private final Body body;
  private final RequestTemplate requestTemplate;

  /**
   * Creates a new Request.
   *
   * @param method of the request.
   * @param url for the request.
   * @param headers for the request.
   * @param body for the request, optional.
   * @param requestTemplate used to build the request.
   */
  Request(HttpMethod method,
      String url,
      Map<String, Collection<String>> headers,
      Body body,
      RequestTemplate requestTemplate) {
    this.httpMethod = checkNotNull(method, "httpMethod of %s", method.name());
    this.url = checkNotNull(url, "url");
    this.headers = checkNotNull(headers, "headers of %s %s", method, url);
    this.body = body;
    this.requestTemplate = requestTemplate;
  }
```



#### org.springframework.cloud.netflix.ribbon.SpringClientFactory#getClientConfig

```java
/**
	 * Get the client config associated with the name.
	 * @param name name to search by
	 * @return {@link IClientConfig} instance
	 * @throws RuntimeException if any error occurs
	 */
	public IClientConfig getClientConfig(String name) {
		return getInstance(name, IClientConfig.class);
	}

```



#### org.springframework.cloud.netflix.ribbon.SpringClientFactory#getInstance



```java
@Override
	public <C> C getInstance(String name, Class<C> type) {
		C instance = super.getInstance(name, type);
		if (instance != null) {
			return instance;
		}
		IClientConfig config = getInstance(name, IClientConfig.class);
		return instantiateWithConfig(getContext(name), type, config);
	}
```



### 第一次feign调用



#### org.springframework.cloud.context.named.NamedContextFactory#getInstance(java.lang.String, java.lang.Class<T>)

```java
public <T> T getInstance(String name, Class<T> type) {
		AnnotationConfigApplicationContext context = getContext(name);
		if (BeanFactoryUtils.beanNamesForTypeIncludingAncestors(context,
				type).length > 0) {
			return context.getBean(type);
		}
		return null;
	}
```



#### org.springframework.cloud.context.named.NamedContextFactory#getContext



```java
protected AnnotationConfigApplicationContext getContext(String name) {
		if (!this.contexts.containsKey(name)) {
			synchronized (this.contexts) {
				if (!this.contexts.containsKey(name)) {
					this.contexts.put(name, createContext(name));
				}
			}
		}
		return this.contexts.get(name);
	}
```

#### org.springframework.cloud.context.named.NamedContextFactory#createContext



```java
	protected AnnotationConfigApplicationContext createContext(String name) {
    //创建ribbon上下文
		AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
		if (this.configurations.containsKey(name)) {
			for (Class<?> configuration : this.configurations.get(name)
					.getConfiguration()) {
				context.register(configuration);
			}
		}
		for (Map.Entry<String, C> entry : this.configurations.entrySet()) {
			if (entry.getKey().startsWith("default.")) {
				for (Class<?> configuration : entry.getValue().getConfiguration()) {
					context.register(configuration);
				}
			}
		}
		context.register(PropertyPlaceholderAutoConfiguration.class,
				this.defaultConfigType);
		context.getEnvironment().getPropertySources().addFirst(new MapPropertySource(
				this.propertySourceName,
				Collections.<String, Object>singletonMap(this.propertyName, name)));
		if (this.parent != null) {
			// Uses Environment from parent as well as beans
			context.setParent(this.parent);
			// jdk11 issue
			// https://github.com/spring-cloud/spring-cloud-netflix/issues/3101
			context.setClassLoader(this.parent.getClassLoader());
		}
		context.setDisplayName(generateDisplayName(name));
		context.refresh();
		return context;
	}

```



![image-20220228162747644](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228162747644.png)



#### `ribbon上下文中的BeanDefinition 处理，并通过refresh进行对象的初始化`

![image-20220228163021709](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228163021709.png)



![image-20220228164143113](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228164143113.png)

`BeanDefinition已经进行了注册` 查看 对应的ribbon 的 `AnnotationConfigApplicationContext`开始初始化如下信息: 

**RibbonClientConfiguration**

**NacosRibbonClientConfiguration**



#### 初始化RibbonClientConfiguration中 IClientConfig ribbonClientConfig

注意：`.RibbonClientConfiguration`类中  这里面虽然`@Bean`非常多，但是这里这使用了`IClientConfig ribbonClientConfig()`，源码中只看到初始化这个： 



org.springframework.cloud.netflix.ribbon.RibbonClientConfiguration#ribbonClientConfig

```java
  @Bean
	@ConditionalOnMissingBean
	public IClientConfig ribbonClientConfig() {
		DefaultClientConfigImpl config = new DefaultClientConfigImpl();
		config.loadProperties(this.name);
		config.set(CommonClientConfigKey.ConnectTimeout, DEFAULT_CONNECT_TIMEOUT);
		config.set(CommonClientConfigKey.ReadTimeout, DEFAULT_READ_TIMEOUT);
		config.set(CommonClientConfigKey.GZipPayload, DEFAULT_GZIP_PAYLOAD);
		return config;
	}
```

配置参数的意义，默认值太小了：

```yaml
ribbon:
  ReadTimeout: 150000
  ConnectionTimeout: 5000
```

##### ribbon默认配置：

```properties
"PrimeConnectionsURI" -> "/"
"ClientClassName" -> "com.netflix.niws.client.http.RestClient"
"listOfServers" -> ""
"EnableZoneAffinity" -> "false"
"IsClientAuthRequired" -> "false"
"FollowRedirects" -> "false"
"Port" -> "7001"
"PrioritizeVipAddressBasedServers" -> "true"
"OkToRetryOnAllOperations" -> "false"
"ReadTimeout" -> {Integer@8627} 1000
"NFLoadBalancerClassName" -> "com.netflix.loadbalancer.ZoneAwareLoadBalancer"
"DeploymentContextBasedVipAddresses" -> "micro-provider"
"MaxHttpConnectionsPerHost" -> "50"
"ConnIdleEvictTimeMilliSeconds" -> "30000"
"PrimeConnectionsClassName" -> "com.netflix.niws.client.http.HttpPrimeConnection"
"PoolMinThreads" -> "1"
"ConnectionCleanerRepeatInterval" -> "30000"
"MaxTotalTimeToPrimeConnections" -> "30000"
"NIWSServerListClassName" -> "com.netflix.loadbalancer.ConfigurationBasedServerList"
"ConnectTimeout" -> {Integer@8336} 1000
"MaxRetriesPerServerPrimeConnection" -> "9"
"MaxConnectionsPerHost" -> "50"
"MaxTotalConnections" -> "200"
"ConnectionManagerTimeout" -> "2000"
"MaxAutoRetries" -> "0"
"PoolKeepAliveTime" -> "900"
"UseIPAddrForServer" -> "false"
"VipAddressResolverClassName" -> "com.netflix.client.SimpleVipAddressResolver"
"MaxTotalHttpConnections" -> "200"
"EnableConnectionPool" -> "true"
"PoolMaxThreads" -> "200"
"EnablePrimeConnections" -> "false"
"MinPrimeConnectionsRatio" -> "1.0"
"EnableGZIPContentEncodingFilter" -> "false"
"PoolKeepAliveTimeUnits" -> "SECONDS"
"NFLoadBalancerPingClassName" -> "com.netflix.loadbalancer.DummyPing"
"EnableZoneExclusivity" -> "false"
"ConnectionPoolCleanerTaskEnabled" -> "true"
"MaxAutoRetriesNextServer" -> "1"
"NFLoadBalancerRuleClassName" -> "com.netflix.loadbalancer.AvailabilityFilteringRule"
```



#### com.alibaba.cloud.nacos.ribbon.NacosRibbonClientConfiguration

```java
@Bean
	@ConditionalOnMissingBean
	public ServerList<?> ribbonServerList(IClientConfig config,
			NacosDiscoveryProperties nacosDiscoveryProperties) {
		if (this.propertiesFactory.isSet(ServerList.class, config.getClientName())) {
			ServerList serverList = this.propertiesFactory.get(ServerList.class, config,
					config.getClientName());
			return serverList;
		}
		NacosServerList serverList = new NacosServerList(nacosDiscoveryProperties);
		serverList.initWithNiwsConfig(config);
		return serverList;
	}
```

![image-20220228172119546](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228172119546.png)



#### 自定义的nacos配置开始凸显

![image-20220228172214687](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228172214687.png)





#### org.springframework.cloud.netflix.ribbon.RibbonClientConfiguration

这里面的所有的`@Bean`均会对应的初始化

```java
/*
 * Copyright 2013-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.cloud.netflix.ribbon;

import java.net.URI;

import javax.annotation.PostConstruct;

import com.netflix.client.DefaultLoadBalancerRetryHandler;
import com.netflix.client.RetryHandler;
import com.netflix.client.config.CommonClientConfigKey;
import com.netflix.client.config.DefaultClientConfigImpl;
import com.netflix.client.config.IClientConfig;
import com.netflix.loadbalancer.ConfigurationBasedServerList;
import com.netflix.loadbalancer.DummyPing;
import com.netflix.loadbalancer.ILoadBalancer;
import com.netflix.loadbalancer.IPing;
import com.netflix.loadbalancer.IRule;
import com.netflix.loadbalancer.PollingServerListUpdater;
import com.netflix.loadbalancer.Server;
import com.netflix.loadbalancer.ServerList;
import com.netflix.loadbalancer.ServerListFilter;
import com.netflix.loadbalancer.ServerListUpdater;
import com.netflix.loadbalancer.ZoneAvoidanceRule;
import com.netflix.loadbalancer.ZoneAwareLoadBalancer;
import com.netflix.niws.client.http.RestClient;
import com.sun.jersey.api.client.Client;
import com.sun.jersey.client.apache4.ApacheHttpClient4;
import org.apache.http.client.params.ClientPNames;
import org.apache.http.client.params.CookiePolicy;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.commons.httpclient.HttpClientConfiguration;
import org.springframework.cloud.netflix.ribbon.apache.HttpClientRibbonConfiguration;
import org.springframework.cloud.netflix.ribbon.okhttp.OkHttpRibbonConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import static com.netflix.client.config.CommonClientConfigKey.DeploymentContextBasedVipAddresses;
import static org.springframework.cloud.netflix.ribbon.RibbonUtils.setRibbonProperty;
import static org.springframework.cloud.netflix.ribbon.RibbonUtils.updateToSecureConnectionIfNeeded;

/**
 * @author Dave Syer
 * @author Tim Ysewyn
 */
@SuppressWarnings("deprecation")
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties
// Order is important here, last should be the default, first should be optional
// see
// https://github.com/spring-cloud/spring-cloud-netflix/issues/2086#issuecomment-316281653
@Import({ HttpClientConfiguration.class, OkHttpRibbonConfiguration.class,
		RestClientRibbonConfiguration.class, HttpClientRibbonConfiguration.class })
public class RibbonClientConfiguration {

	/**
	 * Ribbon client default connect timeout.
	 */
	public static final int DEFAULT_CONNECT_TIMEOUT = 1000;

	/**
	 * Ribbon client default read timeout.
	 */
	public static final int DEFAULT_READ_TIMEOUT = 1000;

	/**
	 * Ribbon client default Gzip Payload flag.
	 */
	public static final boolean DEFAULT_GZIP_PAYLOAD = true;

	@RibbonClientName
	private String name = "client";

	// TODO: maybe re-instate autowired load balancers: identified by name they could be
	// associated with ribbon clients

	@Autowired
	private PropertiesFactory propertiesFactory;

	@Bean
	@ConditionalOnMissingBean
	public IClientConfig ribbonClientConfig() {
    //加载ribbon默认的配置信息
		DefaultClientConfigImpl config = new DefaultClientConfigImpl();
		config.loadProperties(this.name);
    //这里默认的connection_timeout 默认是1秒
		config.set(CommonClientConfigKey.ConnectTimeout, DEFAULT_CONNECT_TIMEOUT);
    //这里默认的读超时时间也是1秒
		config.set(CommonClientConfigKey.ReadTimeout, DEFAULT_READ_TIMEOUT);
		config.set(CommonClientConfigKey.GZipPayload, DEFAULT_GZIP_PAYLOAD);
    
		return config;
	}

	@Bean
	@ConditionalOnMissingBean
	public IRule ribbonRule(IClientConfig config) {
		if (this.propertiesFactory.isSet(IRule.class, name)) {
			return this.propertiesFactory.get(IRule.class, config, name);
		}
    //负载均衡的规则信息
		ZoneAvoidanceRule rule = new ZoneAvoidanceRule();
		rule.initWithNiwsConfig(config);
		return rule;
	}

	@Bean
	@ConditionalOnMissingBean
	public IPing ribbonPing(IClientConfig config) {
		if (this.propertiesFactory.isSet(IPing.class, name)) {
			return this.propertiesFactory.get(IPing.class, config, name);
		}
    //探测方式
		return new DummyPing();
	}

	@Bean
	@ConditionalOnMissingBean
	@SuppressWarnings("unchecked")
	public ServerList<Server> ribbonServerList(IClientConfig config) {
		if (this.propertiesFactory.isSet(ServerList.class, name)) {
			return this.propertiesFactory.get(ServerList.class, config, name);
		}
    //初始化服务列表信息
    
		ConfigurationBasedServerList serverList = new ConfigurationBasedServerList();
		serverList.initWithNiwsConfig(config);
		return serverList;
	}

	@Bean
	@ConditionalOnMissingBean
	public ServerListUpdater ribbonServerListUpdater(IClientConfig config) {
    //跟踪这个类，发现定时任务，每隔30秒更新ServeList，可用的调用服务列表，初始化轮训获取服务列表对象
		return new PollingServerListUpdater(config);
	}

	@Bean
	@ConditionalOnMissingBean
	public ILoadBalancer ribbonLoadBalancer(IClientConfig config,
			ServerList<Server> serverList, ServerListFilter<Server> serverListFilter,
			IRule rule, IPing ping, ServerListUpdater serverListUpdater) {
		if (this.propertiesFactory.isSet(ILoadBalancer.class, name)) {
			return this.propertiesFactory.get(ILoadBalancer.class, config, name);
		}
    //初始化
		return new ZoneAwareLoadBalancer<>(config, rule, ping, serverList,
				serverListFilter, serverListUpdater);
	}

	@Bean
	@ConditionalOnMissingBean
	@SuppressWarnings("unchecked")
	public ServerListFilter<Server> ribbonServerListFilter(IClientConfig config) {
		if (this.propertiesFactory.isSet(ServerListFilter.class, name)) {
			return this.propertiesFactory.get(ServerListFilter.class, config, name);
		}
		ZonePreferenceServerListFilter filter = new ZonePreferenceServerListFilter();
		filter.initWithNiwsConfig(config);
    //添加过滤信息
		return filter;
	}

	@Bean
	@ConditionalOnMissingBean
	public RibbonLoadBalancerContext ribbonLoadBalancerContext(ILoadBalancer loadBalancer,
			IClientConfig config, RetryHandler retryHandler) {
    //初始化ribbon负载均衡
		return new RibbonLoadBalancerContext(loadBalancer, config, retryHandler);
	}

	@Bean
	@ConditionalOnMissingBean
	public RetryHandler retryHandler(IClientConfig config) {
		return new DefaultLoadBalancerRetryHandler(config);
	}

	@Bean
	@ConditionalOnMissingBean
	public ServerIntrospector serverIntrospector() {
		return new DefaultServerIntrospector();
	}

	@PostConstruct
	public void preprocess() {
		setRibbonProperty(name, DeploymentContextBasedVipAddresses.key(), name);
	}

	static class OverrideRestClient extends RestClient {

		private IClientConfig config;

		private ServerIntrospector serverIntrospector;

		protected OverrideRestClient(IClientConfig config,
				ServerIntrospector serverIntrospector) {
			super();
			this.config = config;
			this.serverIntrospector = serverIntrospector;
			initWithNiwsConfig(this.config);
		}

		@Override
		public URI reconstructURIWithServer(Server server, URI original) {
			URI uri = updateToSecureConnectionIfNeeded(original, this.config,
					this.serverIntrospector, server);
			return super.reconstructURIWithServer(server, uri);
		}

		@Override
		protected Client apacheHttpClientSpecificInitialization() {
			ApacheHttpClient4 apache = (ApacheHttpClient4) super.apacheHttpClientSpecificInitialization();
			apache.getClientHandler().getHttpClient().getParams().setParameter(
					ClientPNames.COOKIE_POLICY, CookiePolicy.IGNORE_COOKIES);
			return apache;
		}

	}

}

```



#### org.springframework.cloud.netflix.ribbon.RibbonClientConfiguration#ribbonLoadBalancer

```java
@Bean
	@ConditionalOnMissingBean
	public ILoadBalancer ribbonLoadBalancer(IClientConfig config,
			ServerList<Server> serverList, ServerListFilter<Server> serverListFilter,
			IRule rule, IPing ping, ServerListUpdater serverListUpdater) {
		if (this.propertiesFactory.isSet(ILoadBalancer.class, name)) {
			return this.propertiesFactory.get(ILoadBalancer.class, config, name);
		}
    //核心是这句
		return new ZoneAwareLoadBalancer<>(config, rule, ping, serverList,
				serverListFilter, serverListUpdater);
	}
```



#### com.netflix.loadbalancer.ZoneAwareLoadBalancer#ZoneAwareLoadBalancer(com.netflix.client.config.IClientConfig, com.netflix.loadbalancer.IRule, com.netflix.loadbalancer.IPing, com.netflix.loadbalancer.ServerList<T>, com.netflix.loadbalancer.ServerListFilter<T>, com.netflix.loadbalancer.ServerListUpdater)



```java
 public ZoneAwareLoadBalancer(IClientConfig clientConfig, IRule rule,
                                 IPing ping, ServerList<T> serverList, ServerListFilter<T> filter,
                                 ServerListUpdater serverListUpdater) {
        //从超类继承
        super(clientConfig, rule, ping, serverList, filter, serverListUpdater);
    }
```



#### com.netflix.loadbalancer.DynamicServerListLoadBalancer#DynamicServerListLoadBalancer(com.netflix.client.config.IClientConfig, com.netflix.loadbalancer.IRule, com.netflix.loadbalancer.IPing, com.netflix.loadbalancer.ServerList<T>, com.netflix.loadbalancer.ServerListFilter<T>, com.netflix.loadbalancer.ServerListUpdater)



```java
public DynamicServerListLoadBalancer(IClientConfig clientConfig, IRule rule, IPing ping,
                                         ServerList<T> serverList, ServerListFilter<T> filter,
                                         ServerListUpdater serverListUpdater) {
        super(clientConfig, rule, ping);
        this.serverListImpl = serverList;
        this.filter = filter;
        this.serverListUpdater = serverListUpdater;
        if (filter instanceof AbstractServerListFilter) {
            ((AbstractServerListFilter) filter).setLoadBalancerStats(getLoadBalancerStats());
        }
  // 服务列表配置初始化
        restOfInit(clientConfig);
    }
```



#### com.netflix.loadbalancer.DynamicServerListLoadBalancer#restOfInit

```java
void restOfInit(IClientConfig clientConfig) {
        boolean primeConnection = this.isEnablePrimingConnections();
        // turn this off to avoid duplicated asynchronous priming done in BaseLoadBalancer.setServerList()
        this.setEnablePrimingConnections(false);
        enableAndInitLearnNewServersFeature();

        updateListOfServers();
        if (primeConnection && this.getPrimeConnections() != null) {
            this.getPrimeConnections()
                    .primeConnections(getReachableServers());
        }
        this.setEnablePrimingConnections(primeConnection);
        LOGGER.info("DynamicServerListLoadBalancer for client {} initialized: {}", clientConfig.getClientName(), this.toString());
    }
    
```



#### com.netflix.loadbalancer.DynamicServerListLoadBalancer#enableAndInitLearnNewServersFeature

```java
 /**
     * Feature that lets us add new instances (from AMIs) to the list of
     * existing servers that the LB will use Call this method if you want this
     * feature enabled
     */
    public void enableAndInitLearnNewServersFeature() {
        LOGGER.info("Using serverListUpdater {}", serverListUpdater.getClass().getSimpleName());
         //开启定时任务，默认每隔30秒更新一下，服务列表信息
        serverListUpdater.start(updateAction);
    }

```



#### com.netflix.loadbalancer.PollingServerListUpdater#start



```java
 @Override
    public synchronized void start(final UpdateAction updateAction) {
        if (isActive.compareAndSet(false, true)) {
            final Runnable wrapperRunnable = new Runnable() {
                @Override
                public void run() {
                    if (!isActive.get()) {
                        if (scheduledFuture != null) {
                            scheduledFuture.cancel(true);
                        }
                        return;
                    }
                    try {
                        updateAction.doUpdate();
                        lastUpdated = System.currentTimeMillis();
                    } catch (Exception e) {
                        logger.warn("Failed one update cycle", e);
                    }
                }
            };
             //创建定时任务，每隔30秒执行一次，刷新服务列表信息
            scheduledFuture = getRefreshExecutor().scheduleWithFixedDelay(
                    wrapperRunnable,
                    initialDelayMs,
                    refreshIntervalMs,
                    TimeUnit.MILLISECONDS
            );
        } else {
            logger.info("Already active, no-op");
        }
    }
```



![image-20220228174654082](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228174654082.png)



#### com.netflix.loadbalancer.DynamicServerListLoadBalancer#DynamicServerListLoadBalancer(com.netflix.client.config.IClientConfig)



```java
public DynamicServerListLoadBalancer(IClientConfig clientConfig) {
        this.isSecure = false;
        this.useTunnel = false;
        this.serverListUpdateInProgress = new AtomicBoolean(false);

        //这里内部类进行了实现UpdateAction这个接口
        class NamelessClass_1 implements UpdateAction {
            NamelessClass_1() {
            }

            public void doUpdate() {
               // 真正做服务列表更新的是这句
                DynamicServerListLoadBalancer.this.updateListOfServers();
            }
        }

        this.updateAction = new NamelessClass_1();
        this.initWithNiwsConfig(clientConfig);
    }
```



#### com.netflix.loadbalancer.DynamicServerListLoadBalancer#updateListOfServers



```java
    @VisibleForTesting
    public void updateListOfServers() {
        List<T> servers = new ArrayList();
        if (this.serverListImpl != null) {
           //获取更新的服务列表
            servers = this.serverListImpl.getUpdatedListOfServers();
            LOGGER.debug("List of Servers for {} obtained from Discovery client: {}", this.getIdentifier(), servers);
            if (this.filter != null) {
              //如果有过滤器，再使用过滤器进行一层的更新
                servers = this.filter.getFilteredListOfServers((List)servers);
                LOGGER.debug("Filtered List of Servers for {} obtained from Discovery client: {}", this.getIdentifier(), servers);
            }
        }

        this.updateAllServerList((List)servers);
    }
```



对应的debug信息如下： 

![image-20220228175033534](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228175033534.png)



继续深入更新



![image-20220228175328046](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228175328046.png)

#### com.alibaba.cloud.nacos.ribbon.NacosServerList#getUpdatedListOfServers

```java
@Override
	public List<NacosServer> getUpdatedListOfServers() {
		return getServers();
	}

```



#### 服务注册发现的ribbon和nacos的核心点



#### com.alibaba.cloud.nacos.ribbon.NacosServerList#getServers

```java
private List<NacosServer> getServers() {
		try {
      //获取分组信息 micro-spring-cloud分组，这个是nacos中 group的概念
			String group = discoveryProperties.getGroup();
      //获取对应group下的 serverId 为 micro-provider的服务列表
			List<Instance> instances = discoveryProperties.namingServiceInstance()
					.selectInstances(serviceId, group, true);
			return instancesToServerList(instances);
		}
		catch (Exception e) {
			throw new IllegalStateException(
					"Can not get service instances from nacos, serviceId=" + serviceId,
					e);
		}
	}
```

![image-20220228175854986](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228175854986.png)





#### com.alibaba.cloud.nacos.ribbon.NacosServerList#instancesToServerList

```java
private List<NacosServer> instancesToServerList(List<Instance> instances) {
		List<NacosServer> result = new ArrayList<>();
		if (CollectionUtils.isEmpty(instances)) {
			return result;
		}
		for (Instance instance : instances) {
      //把对应的实例列表信息，封装为了List<NacosServer>返回
			result.add(new NacosServer(instance));
		}

		return result;
	}
```



#### 对应的服务结果如下： 

![image-20220228180110051](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228180110051.png)





#### 再次回到com.netflix.loadbalancer.DynamicServerListLoadBalancer#updateListOfServers

```java
 @VisibleForTesting
    public void updateListOfServers() {
        List<T> servers = new ArrayList<T>();
        if (serverListImpl != null) {
            //获取服务列表信息
            servers = serverListImpl.getUpdatedListOfServers();
            LOGGER.debug("List of Servers for {} obtained from Discovery client: {}",
                    getIdentifier(), servers);

            if (filter != null) {
              //经过过滤以后的信息
                servers = filter.getFilteredListOfServers(servers);
                LOGGER.debug("Filtered List of Servers for {} obtained from Discovery client: {}",
                        getIdentifier(), servers);
            }
        }
       //更新服务列表
        updateAllServerList(servers);
    }
```



#### com.netflix.loadbalancer.DynamicServerListLoadBalancer#updateAllServerList

```java
  /**
     * Update the AllServer list in the LoadBalancer if necessary and enabled
     * 
     * @param ls
     */
    protected void updateAllServerList(List<T> ls) {
        // other threads might be doing this - in which case, we pass
        if (serverListUpdateInProgress.compareAndSet(false, true)) {
            try {
                for (T s : ls) {
                    s.setAlive(true); // set so that clients can start using these
                                      // servers right away instead
                                      // of having to wait out the ping cycle.
                }
              //设置服务列表更新
                setServersList(ls);
                super.forceQuickPing();
            } finally {
                serverListUpdateInProgress.set(false);
            }
        }
    }

```



#### com.netflix.loadbalancer.DynamicServerListLoadBalancer#setServersList

```java
@Override
    public void setServersList(List lsrv) {
        super.setServersList(lsrv);
        List<T> serverList = (List<T>) lsrv;
        Map<String, List<Server>> serversInZones = new HashMap<String, List<Server>>();
        for (Server server : serverList) {
            // make sure ServerStats is created to avoid creating them on hot
            // path
            getLoadBalancerStats().getSingleServerStat(server);
            String zone = server.getZone();
            if (zone != null) {
                zone = zone.toLowerCase();
                List<Server> servers = serversInZones.get(zone);
                if (servers == null) {
                    servers = new ArrayList<Server>();
                    serversInZones.put(zone, servers);
                }
                servers.add(server);
            }
        }
        setServerListForZones(serversInZones);
    }

```

执行结果:

![image-20220228180819309](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228180819309.png)



#### org.springframework.cloud.netflix.ribbon.RibbonClientConfiguration#ribbonLoadBalancerContext

```java
@Bean
	@ConditionalOnMissingBean
	public RibbonLoadBalancerContext ribbonLoadBalancerContext(ILoadBalancer loadBalancer,
			IClientConfig config, RetryHandler retryHandler) {
		return new RibbonLoadBalancerContext(loadBalancer, config, retryHandler);
	}
```

初始化了`LoadBalancerContext`

![image-20220228181108139](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228181108139.png)



#### com.netflix.loadbalancer.ZoneAwareLoadBalancer#setServerListForZones

```JAVA
@Override
    protected void setServerListForZones(Map<String, List<Server>> zoneServersMap) {
        super.setServerListForZones(zoneServersMap);
        if (balancers == null) {
            balancers = new ConcurrentHashMap<String, BaseLoadBalancer>();
        }
        for (Map.Entry<String, List<Server>> entry: zoneServersMap.entrySet()) {
        	String zone = entry.getKey().toLowerCase();
            getLoadBalancer(zone).setServersList(entry.getValue());
        }
        // check if there is any zone that no longer has a server
        // and set the list to empty so that the zone related metrics does not
        // contain stale data
        for (Map.Entry<String, BaseLoadBalancer> existingLBEntry: balancers.entrySet()) {
            if (!zoneServersMap.keySet().contains(existingLBEntry.getKey())) {
                existingLBEntry.getValue().setServersList(Collections.emptyList());
            }
        }
    }    
```





#### com.netflix.loadbalancer.BaseLoadBalancer#setServersList

```java
  /**
     * Set the list of servers used as the server pool. This overrides existing
     * server list.
     */
    public void setServersList(List lsrv) {
      //读写锁更新服务列表信息
        Lock writeLock = allServerLock.writeLock();
        logger.debug("LoadBalancer [{}]: clearing server list (SET op)", name);
        
        ArrayList<Server> newServers = new ArrayList<Server>();
        writeLock.lock();
        try {
            ArrayList<Server> allServers = new ArrayList<Server>();
            for (Object server : lsrv) {
                if (server == null) {
                    continue;
                }

                if (server instanceof String) {
                    server = new Server((String) server);
                }

                if (server instanceof Server) {
                    logger.debug("LoadBalancer [{}]:  addServer [{}]", name, ((Server) server).getId());
                    allServers.add((Server) server);
                } else {
                    throw new IllegalArgumentException(
                            "Type String or Server expected, instead found:"
                                    + server.getClass());
                }

            }
            boolean listChanged = false;
            if (!allServerList.equals(allServers)) {
                listChanged = true;
                if (changeListeners != null && changeListeners.size() > 0) {
                   List<Server> oldList = ImmutableList.copyOf(allServerList);
                   List<Server> newList = ImmutableList.copyOf(allServers);                   
                   for (ServerListChangeListener l: changeListeners) {
                       try {
                           l.serverListChanged(oldList, newList);
                       } catch (Exception e) {
                           logger.error("LoadBalancer [{}]: Error invoking server list change listener", name, e);
                       }
                   }
                }
            }
            if (isEnablePrimingConnections()) {
                for (Server server : allServers) {
                    if (!allServerList.contains(server)) {
                        server.setReadyToServe(false);
                        newServers.add((Server) server);
                    }
                }
                if (primeConnections != null) {
                    primeConnections.primeConnectionsAsync(newServers, this);
                }
            }
            // This will reset readyToServe flag to true on all servers
            // regardless whether
            // previous priming connections are success or not
            allServerList = allServers;
            if (canSkipPing()) {
                for (Server s : allServerList) {
                    s.setAlive(true);
                }
                upServerList = allServerList;
            } else if (listChanged) {
                forceQuickPing();
            }
        } finally {
            writeLock.unlock();
        }
    }
```

执行结果：

![image-20220228181451769](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228181451769.png)



#### 继续回到： org.springframework.cloud.openfeign.ribbon.LoadBalancerFeignClient#execute

```java
@Override
	public Response execute(Request request, Request.Options options) throws IOException {
		try {
			URI asUri = URI.create(request.url());
			String clientName = asUri.getHost();
			URI uriWithoutHost = cleanUrl(request.url(), clientName);
			FeignLoadBalancer.RibbonRequest ribbonRequest = new FeignLoadBalancer.RibbonRequest(
					this.delegate, request, uriWithoutHost);
      //获取IClientConfig，过程中是以上新建的ribbon上下文，等的配置更新
			IClientConfig requestConfig = getClientConfig(options, clientName);   
      //核心处理
			return lbClient(clientName)
					.executeWithLoadBalancer(ribbonRequest, requestConfig).toResponse();
		}
		catch (ClientException e) {
			IOException io = findIOException(e);
			if (io != null) {
				throw io;
			}
			throw new RuntimeException(e);
		}
	}
```



#### org.springframework.cloud.openfeign.ribbon.CachingSpringLoadBalancerFactory#create

```java
public FeignLoadBalancer create(String clientName) {
		FeignLoadBalancer client = this.cache.get(clientName);
		if (client != null) {
			return client;
		}
		IClientConfig config = this.factory.getClientConfig(clientName);
		ILoadBalancer lb = this.factory.getLoadBalancer(clientName);
  //服务的安全访问类型，比如端口443，或者8443
		ServerIntrospector serverIntrospector = this.factory.getInstance(clientName,
				ServerIntrospector.class);
		client = this.loadBalancedRetryFactory != null
				? new RetryableFeignLoadBalancer(lb, config, serverIntrospector,
						this.loadBalancedRetryFactory)
				: new FeignLoadBalancer(lb, config, serverIntrospector);
  //最后得到FeignLoadBalancer，并缓存
		this.cache.put(clientName, client);
		return client;
	}
```



![image-20220228183535529](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228183535529.png)







#### com.netflix.client.AbstractLoadBalancerAwareClient#executeWithLoadBalancer(S, com.netflix.client.config.IClientConfig)

```java
  /**
     * This method should be used when the caller wants to dispatch the request to a server chosen by
     * the load balancer, instead of specifying the server in the request's URI. 
     * It calculates the final URI by calling {@link #reconstructURIWithServer(com.netflix.loadbalancer.Server, java.net.URI)}
     * and then calls {@link #executeWithLoadBalancer(ClientRequest, com.netflix.client.config.IClientConfig)}.
     * 
     * @param request request to be dispatched to a server chosen by the load balancer. The URI can be a partial
     * URI which does not contain the host name or the protocol.
     */
    public T executeWithLoadBalancer(final S request, final IClientConfig requestConfig) throws ClientException {
        LoadBalancerCommand<T> command = buildLoadBalancerCommand(request, requestConfig);

        try {
            return command.submit(
                new ServerOperation<T>() {
                    @Override
                    public Observable<T> call(Server server) {
                        URI finalUri = reconstructURIWithServer(server, request.getUri());
                        S requestForServer = (S) request.replaceUri(finalUri);
                        try {
                            return Observable.just(AbstractLoadBalancerAwareClient.this.execute(requestForServer, requestConfig));
                        } 
                        catch (Exception e) {
                            return Observable.error(e);
                        }
                    }
                })
                .toBlocking()
                .single();
        } catch (Exception e) {
            Throwable t = e.getCause();
            if (t instanceof ClientException) {
                throw (ClientException) t;
            } else {
                throw new ClientException(e);
            }
        }
        
    }
```





#### com.netflix.loadbalancer.reactive.LoadBalancerCommand#submit

#### com.netflix.loadbalancer.reactive.LoadBalancerCommand#submit

```java

```



这里面采用了RXJava进行调用，有点复杂，这里只列出核心处理

#### 

```java
   final int maxRetrysSame = retryHandler.getMaxRetriesOnSameServer();
        final int maxRetrysNext = retryHandler.getMaxRetriesOnNextServer();

        // Use the load balancer
        Observable<T> o = 
                (server == null ? selectServer() : Observable.just(server))
                .concatMap(new Func1<Server, Observable<T>>() {
                    @Override
                    // Called for each server being selected
                    public Observable<T> call(Server server) {
                        context.setServer(server);
                        final ServerStats stats = loadBalancerContext.getServerStats(server);
                        
                        // Called for each attempt and retry
                        Observable<T> o = Observable
                                .just(server)
                                .concatMap(new Func1<Server, Observable<T>>() {
                                    @Override
                                    public Observable<T> call(final Server server) {
                                        context.incAttemptCount();
                                        loadBalancerContext.noteOpenConnection(stats);
                                        
                                        if (listenerInvoker != null) {
                                            try {
                                                listenerInvoker.onStartWithServer(context.toExecutionInfo());
                                            } catch (AbortExecutionException e) {
                                                return Observable.error(e);
                                            }
                                        }
                                        
                                        final Stopwatch tracer = loadBalancerContext.getExecuteTracer().start();
                                        
                     
```



#### com.netflix.loadbalancer.reactive.LoadBalancerCommand#selectServer

```java
/**
 * Return an Observable that either emits only the single requested server
 * or queries the load balancer for the next server on each subscription
 */
private Observable<Server> selectServer() {
    return Observable.create(new OnSubscribe<Server>() {
        @Override
        public void call(Subscriber<? super Server> next) {
            try {
                Server server = loadBalancerContext.getServerFromLoadBalancer(loadBalancerURI, loadBalancerKey);   
                next.onNext(server);
                next.onCompleted();
            } catch (Exception e) {
                next.onError(e);
            }
        }
    });
}
```



#### com.netflix.loadbalancer.ZoneAwareLoadBalancer#chooseServer

```java
@Override
    public Server chooseServer(Object key) {
        if (!ENABLED.get() || getLoadBalancerStats().getAvailableZones().size() <= 1) {
            logger.debug("Zone aware logic disabled or there is only one zone");
            return super.chooseServer(key);
        }
        Server server = null;
        try {
            LoadBalancerStats lbStats = getLoadBalancerStats();
            Map<String, ZoneSnapshot> zoneSnapshot = ZoneAvoidanceRule.createSnapshot(lbStats);
            logger.debug("Zone snapshots: {}", zoneSnapshot);
            if (triggeringLoad == null) {
                triggeringLoad = DynamicPropertyFactory.getInstance().getDoubleProperty(
                        "ZoneAwareNIWSDiscoveryLoadBalancer." + this.getName() + ".triggeringLoadPerServerThreshold", 0.2d);
            }

            if (triggeringBlackoutPercentage == null) {
                triggeringBlackoutPercentage = DynamicPropertyFactory.getInstance().getDoubleProperty(
                        "ZoneAwareNIWSDiscoveryLoadBalancer." + this.getName() + ".avoidZoneWithBlackoutPercetage", 0.99999d);
            }
            Set<String> availableZones = ZoneAvoidanceRule.getAvailableZones(zoneSnapshot, triggeringLoad.get(), triggeringBlackoutPercentage.get());
            logger.debug("Available zones: {}", availableZones);
            if (availableZones != null &&  availableZones.size() < zoneSnapshot.keySet().size()) {
                String zone = ZoneAvoidanceRule.randomChooseZone(zoneSnapshot, availableZones);
                logger.debug("Zone chosen: {}", zone);
                if (zone != null) {
                    BaseLoadBalancer zoneLoadBalancer = getLoadBalancer(zone);
                    server = zoneLoadBalancer.chooseServer(key);
                }
            }
        } catch (Exception e) {
            logger.error("Error choosing server using zone aware logic for load balancer={}", name, e);
        }
        if (server != null) {
            return server;
        } else {
            logger.debug("Zone avoidance logic is not invoked.");
            return super.chooseServer(key);
        }
    }
     
```



#### com.netflix.client.AbstractLoadBalancerAwareClient#executeWithLoadBalancer(S, com.netflix.client.config.IClientConfig)

完成URL的重构工作，

```java
ublic T executeWithLoadBalancer(final S request, final IClientConfig requestConfig) throws ClientException {
        LoadBalancerCommand command = this.buildLoadBalancerCommand(request, requestConfig);

        try {
            return (IResponse)command.submit(new ServerOperation<T>() {
                public Observable<T> call(Server server) {
                  //从server转换为URL信息
                    URI finalUri = AbstractLoadBalancerAwareClient.this.reconstructURIWithServer(server, request.getUri());
                    ClientRequest requestForServer = request.replaceUri(finalUri);

                    try {
                      //请求调用
                        return Observable.just(AbstractLoadBalancerAwareClient.this.execute(requestForServer, requestConfig));
                    } catch (Exception var5) {
                        return Observable.error(var5);
                    }
                }
            }).toBlocking().single();
        } catch (Exception var6) {
            Throwable t = var6.getCause();
            if (t instanceof ClientException) {
                throw (ClientException)t;
            } else {
                throw new ClientException(var6);
            }
        }
    }
```

![image-20220228200234952](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228200234952.png)

#### 真正去执行请求org.springframework.cloud.openfeign.ribbon.FeignLoadBalancer#execute

```java
@Override
	public RibbonResponse execute(RibbonRequest request, IClientConfig configOverride)
			throws IOException {
		Request.Options options;
		if (configOverride != null) {
			RibbonProperties override = RibbonProperties.from(configOverride);
			options = new Request.Options(override.connectTimeout(this.connectTimeout),
					override.readTimeout(this.readTimeout));
		}
		else {
			options = new Request.Options(this.connectTimeout, this.readTimeout);
		}
		Response response = request.client().execute(request.toRequest(), options);
		return new RibbonResponse(request.getUri(), response);
	}
```

#### feign.Client.Default#execute

最原始的     ` HttpURLConnection connection = convertAndSend(request, options);`

```java
 @Override
    public Response execute(Request request, Options options) throws IOException {
      HttpURLConnection connection = convertAndSend(request, options);
      return convertResponse(connection, request);
    }

```



#### feign.Client.Default#convertAndSend

```java
HttpURLConnection convertAndSend(Request request, Options options) throws IOException {
      final URL url = new URL(request.url());
      final HttpURLConnection connection = this.getConnection(url);
      if (connection instanceof HttpsURLConnection) {
        HttpsURLConnection sslCon = (HttpsURLConnection) connection;
        if (sslContextFactory != null) {
          sslCon.setSSLSocketFactory(sslContextFactory);
        }
        if (hostnameVerifier != null) {
          sslCon.setHostnameVerifier(hostnameVerifier);
        }
      }
      connection.setConnectTimeout(options.connectTimeoutMillis());
      connection.setReadTimeout(options.readTimeoutMillis());
      connection.setAllowUserInteraction(false);
      connection.setInstanceFollowRedirects(options.isFollowRedirects());
      connection.setRequestMethod(request.httpMethod().name());

      Collection<String> contentEncodingValues = request.headers().get(CONTENT_ENCODING);
      boolean gzipEncodedRequest =
          contentEncodingValues != null && contentEncodingValues.contains(ENCODING_GZIP);
      boolean deflateEncodedRequest =
          contentEncodingValues != null && contentEncodingValues.contains(ENCODING_DEFLATE);

      boolean hasAcceptHeader = false;
      Integer contentLength = null;
      for (String field : request.headers().keySet()) {
        if (field.equalsIgnoreCase("Accept")) {
          hasAcceptHeader = true;
        }
        for (String value : request.headers().get(field)) {
          if (field.equals(CONTENT_LENGTH)) {
            if (!gzipEncodedRequest && !deflateEncodedRequest) {
              contentLength = Integer.valueOf(value);
              connection.addRequestProperty(field, value);
            }
          } else {
            connection.addRequestProperty(field, value);
          }
        }
      }
      // Some servers choke on the default accept string.
      if (!hasAcceptHeader) {
        connection.addRequestProperty("Accept", "*/*");
      }

      if (request.body() != null) {
        if (disableRequestBuffering) {
          if (contentLength != null) {
            connection.setFixedLengthStreamingMode(contentLength);
          } else {
            connection.setChunkedStreamingMode(8196);
          }
        }
        connection.setDoOutput(true);
        OutputStream out = connection.getOutputStream();
        if (gzipEncodedRequest) {
          out = new GZIPOutputStream(out);
        } else if (deflateEncodedRequest) {
          out = new DeflaterOutputStream(out);
        }
        try {
          out.write(request.body());
        } finally {
          try {
            out.close();
          } catch (IOException suppressed) { // NOPMD
          }
        }
      }
      return connection;
    }
  }
```



#### com.netflix.client.IClient   有很多实现类，但是用的是第一个

![image-20220228201358437](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228201358437.png)



### 总结

简单总结一下：  feign 其实就是封装了restTempate的http调用，url大概是 http://micro-provider/provider/ping  ，feign中有自己的子容器，`FeignContext` 本身是个对象，里面有属性

```java
public class FeignContext extends NamedContextFactory<FeignClientSpecification> {
         	private Map<String, AnnotationConfigApplicationContext> contexts = new ConcurrentHashMap<>();

这个map中的value，每一个就是一个@FeignClient的子容器，里面封装了很多feign相关的信息
```

Feign 内含RestTemplate进行调用，得到的url大致为：   http://micro-provider/provider/ping  这种格式： 

里面的 `micro-provider` 如何获取，如何填充为 http://192.168.160.160:8888/provider/ping   这种形式去调用呢？？？

Micro-provider ----->192.168.160.160:8888的过程： 

涉及到负载均衡----> ribbonLoadBalanceContext，

#### feign和ribbon是如何关联的呢？？？？？？？

这张图，是feign和ribbon的关系调用，在feign子容器`AnnotationConfigApplicationContext`中，有个属性configurations，`private Map<String, C> configurations = new ConcurrentHashMap<>();` 也在类`NamedContextFactory` 中定义，这里面就是ribbion的真正的实现类： 

![image-20220228145430481](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228145430481.png)



FeignContext ---> LoadBalancerFeignClient---->nacosServerList

```java
public class LoadBalancerFeignClient implements Client {

```

#### ribbon 是如何和nacos或者Eureka建立关系的？？？

`com.netflix.loadbalancer.ServerList`  这个类的实现，就代表了不同的注册中心

```java
/**
 * Interface that defines the methods sed to obtain the List of Servers
 * @author stonse
 *
 * @param <T>
 */
public interface ServerList<T extends Server> {

    public List<T> getInitialListOfServers();
    
    /**
     * Return updated list of servers. This is called say every 30 secs
     * (configurable) by the Loadbalancer's Ping cycle
     * 
     */
    public List<T> getUpdatedListOfServers();   

}

```

![image-20220228190746144](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220228190746144.png)



**`com.netflix.loadbalancer.ServerList`  这个类的实现，就代表了不同的注册中心**，从不同的注册中心获取服务列表































#### bug分析

##### feign.RequestTemplate#from

```java
/**
   * Create a Request Template from an existing Request Template.
   *
   * @param requestTemplate to copy from.
   * @return a new Request Template.
   */
  public static RequestTemplate from(RequestTemplate requestTemplate) {
    RequestTemplate template =
        new RequestTemplate(
            requestTemplate.target,
            requestTemplate.fragment,
            requestTemplate.uriTemplate,
            requestTemplate.bodyTemplate,
            requestTemplate.method,
            requestTemplate.charset,
            requestTemplate.body,
            requestTemplate.decodeSlash,
            requestTemplate.collectionFormat,
            requestTemplate.methodMetadata,
            requestTemplate.feignTarget);

    if (!requestTemplate.queries().isEmpty()) {
      template.queries.putAll(requestTemplate.queries);
    }

    if (!requestTemplate.headers().isEmpty()) {
      template.headers.putAll(requestTemplate.headers);
    }
    return template;
  }
```

##### feign.RequestTemplate#resolve(java.util.Map<java.lang.String,?>)

```java
 /**
   * Resolve all expressions using the variable value substitutions provided. Variable values will
   * be pct-encoded, if they are not already.
   *
   * @param variables containing the variable values to use when resolving expressions.
   * @return a new Request Template with all of the variables resolved.
   */
  public RequestTemplate resolve(Map<String, ?> variables) {

    StringBuilder uri = new StringBuilder();

    /* create a new template form this one, but explicitly */
    RequestTemplate resolved = RequestTemplate.from(this);

    if (this.uriTemplate == null) {
      /* create a new uri template using the default root */
      this.uriTemplate = UriTemplate.create("", !this.decodeSlash, this.charset);
    }

    String expanded = this.uriTemplate.expand(variables);
    if (expanded != null) {
      uri.append(expanded);
    }

    /*
     * for simplicity, combine the queries into the uri and use the resulting uri to seed the
     * resolved template.
     */
    if (!this.queries.isEmpty()) {
      /*
       * since we only want to keep resolved query values, reset any queries on the resolved copy
       */
      resolved.queries(Collections.emptyMap());
      StringBuilder query = new StringBuilder();
      Iterator<QueryTemplate> queryTemplates = this.queries.values().iterator();

      while (queryTemplates.hasNext()) {
        QueryTemplate queryTemplate = queryTemplates.next();
        String queryExpanded = queryTemplate.expand(variables);
        if (Util.isNotBlank(queryExpanded)) {
          query.append(queryExpanded);
          if (queryTemplates.hasNext()) {
            query.append("&");
          }
        }
      }

      String queryString = query.toString();
      if (!queryString.isEmpty()) {
        Matcher queryMatcher = QUERY_STRING_PATTERN.matcher(uri);
        if (queryMatcher.find()) {
          /* the uri already has a query, so any additional queries should be appended */
          uri.append("&");
        } else {
          uri.append("?");
        }
        uri.append(queryString);
      }
    }

    /* add the uri to result */
    resolved.uri(uri.toString());

    /* headers */
    if (!this.headers.isEmpty()) {
      /*
       * same as the query string, we only want to keep resolved values, so clear the header map on
       * the resolved instance
       */
      resolved.headers(Collections.emptyMap());
      for (HeaderTemplate headerTemplate : this.headers.values()) {
        /* resolve the header */
        String header = headerTemplate.expand(variables);
        if (!header.isEmpty()) {
          /* split off the header values and add it to the resolved template */
          String headerValues = header.substring(header.indexOf(" ") + 1);
          if (!headerValues.isEmpty()) {
            /* append the header as a new literal as the value has already been expanded. */
            resolved.header(headerTemplate.getName(), Literal.create(headerValues));
          }
        }
      }
    }

    if (this.bodyTemplate != null) {
      resolved.body(this.bodyTemplate.expand(variables));
    }

    /* mark the new template resolved */
    resolved.resolved = true;
    return resolved;
  }
```



