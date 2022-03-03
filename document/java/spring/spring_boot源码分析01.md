## 注解分析

### @AutoConfigurationPackage

`@AutoConfigurationPackage`注解，中 `/org/springframework/boot/autoconfigure/AutoConfigurationPackages.java:120` 为了获取基本的注入包，比如我的例子是 `com.xs.micro.boot `



`@AutoConfigurationPackage`注解最终会走向这里

```java
public static void register(BeanDefinitionRegistry registry, String... packageNames) {
		if (registry.containsBeanDefinition(BEAN)) {
			BeanDefinition beanDefinition = registry.getBeanDefinition(BEAN);
			ConstructorArgumentValues constructorArguments = beanDefinition.getConstructorArgumentValues();
			constructorArguments.addIndexedArgumentValue(0, addBasePackages(constructorArguments, packageNames));
		}
		else {
			GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
			beanDefinition.setBeanClass(BasePackages.class);
			beanDefinition.getConstructorArgumentValues().addIndexedArgumentValue(0, packageNames);
			beanDefinition.setRole(BeanDefinition.ROLE_INFRASTRUCTURE);
			registry.registerBeanDefinition(BEAN, beanDefinition);
		}
	}
```



`@Import(AutoConfigurationImportSelector.class)`   分析：  `org.springframework.boot.autoconfigure.AutoConfigurationImportSelector#selectImports`

最终会走向这里：

```java
/**
	 * Return the {@link AutoConfigurationEntry} based on the {@link AnnotationMetadata}
	 * of the importing {@link Configuration @Configuration} class.
	 * @param annotationMetadata the annotation metadata of the configuration class
	 * @return the auto-configurations that should be imported
	 */
	protected AutoConfigurationEntry getAutoConfigurationEntry(AnnotationMetadata annotationMetadata) {
		if (!isEnabled(annotationMetadata)) {
			return EMPTY_ENTRY;
		}
		AnnotationAttributes attributes = getAttributes(annotationMetadata);
		List<String> configurations = getCandidateConfigurations(annotationMetadata, attributes);
		configurations = removeDuplicates(configurations);
		Set<String> exclusions = getExclusions(annotationMetadata, attributes);
		checkExcludedClasses(configurations, exclusions);
		configurations.removeAll(exclusions);
		configurations = getConfigurationClassFilter().filter(configurations);
		fireAutoConfigurationImportEvents(configurations, exclusions);
		return new AutoConfigurationEntry(configurations, exclusions);
	}

```



`List<String> configurations = getCandidateConfigurations(annotationMetadata, attributes);` 这一段代码执行如下，并把执行结果是，获取 `META-INFO/spring.factotries` 对应的配置类：

![image-20220210143611047](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220210143611047.png)



### SpringFactoriesLoader



`SpringFactoriesLoader`的spring boot加载的关键SPI能力



![image-20220210143915811](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220210143915811.png)



在SpringApplication构造方法初始化执行时，也会用到`SpringFactoriesLoader`



![image-20220210144150769](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220210144150769.png)



### ServiceLoader

`SpringFacotriesLoader` 和 Java SPI  `ServiceLoader`类似



### SpringFactoriesLoader分析

```java
private static Map<String, List<String>> loadSpringFactories(@Nullable ClassLoader classLoader) {
		MultiValueMap<String, String> result = cache.get(classLoader);
		if (result != null) {
			return result;
		}

		try {
      //获取当前 classpath 下所有jar包中有的 spring.factories 文件，并将它们加载到内存中。
			Enumeration<URL> urls = (classLoader != null ?
					classLoader.getResources(FACTORIES_RESOURCE_LOCATION) :
					ClassLoader.getSystemResources(FACTORIES_RESOURCE_LOCATION));
			result = new LinkedMultiValueMap<>();
			while (urls.hasMoreElements()) {
				URL url = urls.nextElement();
				UrlResource resource = new UrlResource(url);
				Properties properties = PropertiesLoaderUtils.loadProperties(resource);
				for (Map.Entry<?, ?> entry : properties.entrySet()) {
					String factoryTypeName = ((String) entry.getKey()).trim();
					for (String factoryImplementationName : StringUtils.commaDelimitedListToStringArray((String) entry.getValue())) {
						result.add(factoryTypeName, factoryImplementationName.trim());
					}
				}
			}
      //按照类加载器进行缓存
			cache.put(classLoader, result);
			return result;
		}
		catch (IOException ex) {
			throw new IllegalArgumentException("Unable to load factories from location [" +
					FACTORIES_RESOURCE_LOCATION + "]", ex);
		}
	}

```



`MultiValueMap` 本质上是： 

`Map<K,List<V>>`

![image-20220210151132986](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220210151132986.png)



这个加载中加载了哪些文件： 

```shell
rg.springframework.context.ApplicationContextInitializer
org.springframework.context.ApplicationListener
org.springframework.boot.SpringApplicationRunListener
org.springframework.boot.env.EnvironmentPostProcessor
org.springframework.boot.env.PropertySourceLoader
org.springframework.boot.SpringBootExceptionReporter
org.springframework.boot.diagnostics.FailureAnalyzer
org.springframework.beans.BeanInfoFactory
org.springframework.boot.autoconfigure.EnableAutoConfiguration
org.springframework.boot.autoconfigure.EnableAutoConfiguration
org.springframework.boot.autoconfigure.AutoConfigurationImportFilter
org.springframework.boot.autoconfigure.AutoConfigurationImportListener
org.springframework.boot.autoconfigure.template.TemplateAvailabilityProvider
org.springframework.boot.autoconfigure.template.TemplateAvailabilityProvider
```



## SpringApplication.run启动分析



### 两个重要的入口点： 

![image-20220208163256780](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208163256780.png)



### ApplicationContextInitializer实现类

#### ApplicationContextInitializer扩展方式一： 

```java
public class ApplicationInitializerImpl implements ApplicationContextInitializer {
    @Override
    public void initialize(ConfigurableApplicationContext applicationContext) {
        System.out.println(">>>>>>>>>>>ApplicationInitializerImpl......................................");
    }
}
```

```java
@SpringBootApplication
public class MicroBootApplication {
    public static void main(String[] args) {
        //SpringApplication.run(MicroBootApplication.class, args);

        SpringApplication springApplication = new SpringApplication(MicroBootApplication.class);
        springApplication.addInitializers(new ApplicationInitializerImpl());
        springApplication.run(args);
    }

}

```



#### ApplicationContextInitializer扩展方式二：

```java
private static final String PROPERTY_NAME = "context.initializer.classes";
```

在`application.yml` 中添加对应配置： 

```java
context.initializer.classes = xxxxxxxxxxxx.ApplicationInitializerImpl
```





`org.springframework.context.ApplicationContextInitializer`

七个实现类，优先初始化： 

```java
0 = "org.springframework.boot.context.ConfigurationWarningsApplicationContextInitializer"
1 = "org.springframework.boot.context.ContextIdApplicationContextInitializer"
2 = "org.springframework.boot.context.config.DelegatingApplicationContextInitializer"
3 = "org.springframework.boot.rsocket.context.RSocketPortInfoApplicationContextInitializer"
4 = "org.springframework.boot.web.context.ServerPortInfoApplicationContextInitializer"
5 = "org.springframework.boot.autoconfigure.SharedMetadataReaderFactoryContextInitializer"
6 = "org.springframework.boot.autoconfigure.logging.ConditionEvaluationReportLoggingListener"
```

- ConfigurationWarningsApplicationContextInitializer：报告IOC容器的一些常见的错误配置
- ContextIdApplicationContextInitializer：设置Spring应用上下文的ID
- DelegatingApplicationContextInitializer：加载 `application.properties` 中 `context.initializer.classes` 配置的类
- ServerPortInfoApplicationContextInitializer：将内置servlet容器实际使用的监听端口写入到 `Environment` 环境属性中
- SharedMetadataReaderFactoryContextInitializer：创建一个 SpringBoot 和 `ConfigurationClassPostProcessor` 共用的 `CachingMetadataReaderFactory` 对象
- ConditionEvaluationReportLoggingListener：将 `ConditionEvaluationReport` 写入日志



### SpringApplication初始化

```java
@SuppressWarnings({ "unchecked", "rawtypes" })
	public SpringApplication(ResourceLoader resourceLoader, Class<?>... primarySources) {
		this.resourceLoader = resourceLoader;
		Assert.notNull(primarySources, "PrimarySources must not be null");
		this.primarySources = new LinkedHashSet<>(Arrays.asList(primarySources));
		this.webApplicationType = WebApplicationType.deduceFromClasspath();
		setInitializers((Collection) getSpringFactoriesInstances(ApplicationContextInitializer.class));
		setListeners((Collection) getSpringFactoriesInstances(ApplicationListener.class));
		this.mainApplicationClass = deduceMainApplicationClass();
	}
```





![image-20220208163656807](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208163656807.png)



![image-20220208165650573](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208165650573.png)



以上代码处理，只是创建了7个对象，还有一个`EventPublishingRunListener`的对象



问题： 这14个listener什么时间加载进来的？？？

![image-20220208170030866](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208170030866.png)





### spring boot启动之`ApplicationListener`

![image-20220208154348735](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208154348735.png)



### ApplicationListener





- ClearCachesApplicationListener：应用上下文加载完成后对缓存做清除工作
- ParentContextCloserApplicationListener：监听双亲应用上下文的关闭事件并往自己的子应用上下文中传播
- FileEncodingApplicationListener：检测系统文件编码与应用环境编码是否一致，如果系统文件编码和应用环境的编码不同则终止应用启动
- AnsiOutputApplicationListener：根据 `spring.output.ansi.enabled` 参数配置 AnsiOutput
- ConfigFileApplicationListener：从常见的那些约定的位置读取配置文件
- DelegatingApplicationListener：监听到事件后转发给 `application.properties` 中配置的 `context.listener.classes` 的监听器
- ClasspathLoggingApplicationListener：对环境就绪事件 `ApplicationEnvironmentPreparedEvent` 和应用失败事件 `ApplicationFailedEvent` 做出响应
- LoggingApplicationListener：配置 `LoggingSystem`。使用 `logging.config` 环境变量指定的配置或者缺省配置
- LiquibaseServiceLocatorApplicationListener：使用一个可以和 SpringBoot 可执行jar包配合工作的版本替换 LiquibaseServiceLocator
- BackgroundPreinitializer：使用一个后台线程尽早触发一些耗时的初始化任务



```java
/*
 * Copyright 2012-2019 the original author or authors.
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

package org.springframework.boot;

import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.io.support.SpringFactoriesLoader;

/**
 * Listener for the {@link SpringApplication} {@code run} method.
 * {@link SpringApplicationRunListener}s are loaded via the {@link SpringFactoriesLoader}
 * and should declare a public constructor that accepts a {@link SpringApplication}
 * instance and a {@code String[]} of arguments. A new
 * {@link SpringApplicationRunListener} instance will be created for each run.
 *
 * @author Phillip Webb
 * @author Dave Syer
 * @author Andy Wilkinson
 * @since 1.0.0
 */
public interface SpringApplicationRunListener {

	/**
	 * Called immediately when the run method has first started. Can be used for very
	 * early initialization.
	 首次启动run方法时立即调用。可用于非常早期的初始化。
	 */
	default void starting() {
	}

	/**
	 * Called once the environment has been prepared, but before the
	 * {@link ApplicationContext} has been created.
	 准备好环境（Environment构建完成），但在创建ApplicationContext之前调用。
	 * @param environment the environment
	 */
	default void environmentPrepared(ConfigurableEnvironment environment) {
	}

	/**
	 * Called once the {@link ApplicationContext} has been created and prepared, but
	 * before sources have been loaded.
	 在创建和构建ApplicationContext之后，但在加载之前调用
	 * @param context the application context
	 */
	default void contextPrepared(ConfigurableApplicationContext context) {
	}

	/**
	 * Called once the application context has been loaded but before it has been
	 * refreshed.
	 ApplicationContext已加载但在刷新之前调用
	 * @param context the application context
	 */
	default void contextLoaded(ConfigurableApplicationContext context) {
	}

	/**
	 * The context has been refreshed and the application has started but
	 * {@link CommandLineRunner CommandLineRunners} and {@link ApplicationRunner
	 * ApplicationRunners} have not been called.
	 ApplicationContext已刷新，应用程序已启动，但尚未调用CommandLineRunners和ApplicationRunners。
	 * @param context the application context.
	 * @since 2.0.0
	 */
	default void started(ConfigurableApplicationContext context) {
	}

	/**
	 * Called immediately before the run method finishes, when the application context has
	 * been refreshed and all {@link CommandLineRunner CommandLineRunners} and
	 * {@link ApplicationRunner ApplicationRunners} have been called.
	 * @param context the application context.
	 在运行方法彻底完成之前立即调用，刷新ApplicationContext并调用所有CommandLineRunners和ApplicationRunner。
	 * @since 2.0.0
	 */
	default void running(ConfigurableApplicationContext context) {
	}

	/**
	 * Called when a failure occurs when running the application.
	 * @param context the application context or {@code null} if a failure occurred before
	 * the context was created
	 在运行应用程序时失败时调用
	 * @param exception the failure
	 * @since 2.0.0
	 */
	default void failed(ConfigurableApplicationContext context, Throwable exception) {
	}

}

```

#### 思考： 

事件发布中几个作用时间点，后期做一些spring的扩展点时，可以重点考虑，比如： `在运行方法彻底完成之前立即调用，刷新ApplicationContext并调用所有CommandLineRunners和ApplicationRunner。`

实现`CommandLineRunners` 和 `ApplilcationRuner`都是一些扩展点



#### 扩展

`Environment` ---> profile 和 properties

Environment是IOC容器的运行环境，它包括Profile和Properties两大部分，它可由一个到几个激活的Profile共同配置，它的配置可在应用级Bean中获取



Spring中有很多这样的定义，可以作为一个设计框架的参考：



雷同点设计：`ApplicationContext`

```java
public interface ConfigurableApplicationContext extends ApplicationContext, Lifecycle, Closeable {

public abstract class AbstractApplicationContext extends DefaultResourceLoader
		implements ConfigurableApplicationContext {  

public class GenericApplicationContext extends AbstractApplicationContext implements BeanDefinitionRegistry {

public class AnnotationConfigApplicationContext extends GenericApplicationContext implements AnnotationConfigRegistry {
  
```



雷同点设计： `Environment`

```java
public interface ConfigurableEnvironment extends Environment, ConfigurablePropertyResolver {

public abstract class AbstractEnvironment implements ConfigurableEnvironment {

public class StandardEnvironment extends AbstractEnvironment {

public class StandardServletEnvironment extends StandardEnvironment implements ConfigurableWebEnvironment {
```











`public class EventPublishingRunListener implements SpringApplicationRunListener, Ordered `

这个`applicationListeners`是在什么时候初始化的？？？

![image-20220208161919422](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208161919422.png)





`org.springframework.boot.context.event.EventPublishingRunListener` 核心类：初始化的过程： 

这个







`org.springframework.context.event.AbstractApplicationEventMulticaster#addApplicationListener`





### SpringFactoriesLoader



`org.springframework.core.io.support.SpringFactoriesLoader#loadSpringFactories  ` 核心就是

```java
	private static Map<String, List<String>> loadSpringFactories(@Nullable ClassLoader classLoader) {
		MultiValueMap<String, String> result = cache.get(classLoader);
		if (result != null) {
			return result;
		}

		try {
			Enumeration<URL> urls = (classLoader != null ?
					classLoader.getResources(FACTORIES_RESOURCE_LOCATION) :
					ClassLoader.getSystemResources(FACTORIES_RESOURCE_LOCATION));
			result = new LinkedMultiValueMap<>();
			while (urls.hasMoreElements()) {
				URL url = urls.nextElement();
				UrlResource resource = new UrlResource(url);
				Properties properties = PropertiesLoaderUtils.loadProperties(resource);
				for (Map.Entry<?, ?> entry : properties.entrySet()) {
					String factoryTypeName = ((String) entry.getKey()).trim();
					for (String factoryImplementationName : StringUtils.commaDelimitedListToStringArray((String) entry.getValue())) {
						result.add(factoryTypeName, factoryImplementationName.trim());
					}
				}
			}
			cache.put(classLoader, result);
			return result;
		}
		catch (IOException ex) {
			throw new IllegalArgumentException("Unable to load factories from location [" +
					FACTORIES_RESOURCE_LOCATION + "]", ex);
		}
	}

```







URLClassLoader 

![image-20220208180715112](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208180715112.png)





加载类路径下两个url： 

*  spring-cloud-commons 下的 spring.factories文件
*  Spring-cloud-context下的  spring.factories文件

![image-20220208175106916](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208175106916.png)





![image-20220208175324041](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208175324041.png)



#### 加载所有的spring.factories

![image-20220208182632911](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208182632911.png)



##### ApplicationListener中初始化的14个listener从哪里来？？？

理论上就是从spring.factories

![image-20220208182813037](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208182813037.png)



加载`ApplicationListener`： 

![image-20220208183127588](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220208183127588.png)



加载完成以后，开始创建上下文，初始化类： `org.springframework.context.annotation.AnnotationConfigApplicationContext`



先加载类： 

class org.springframework.context.annotation.ConfigurationClassPostProcessor



## 核心初始化SpringApplication.run()

```java
/**
	 * Run the Spring application, creating and refreshing a new
	 * {@link ApplicationContext}.
	 * @param args the application arguments (usually passed from a Java main method)
	 * @return a running {@link ApplicationContext}
	 */
	public ConfigurableApplicationContext run(String... args) {
		StopWatch stopWatch = new StopWatch();
		stopWatch.start();
		ConfigurableApplicationContext context = null;
		Collection<SpringBootExceptionReporter> exceptionReporters = new ArrayList<>();
		configureHeadlessProperty();
		SpringApplicationRunListeners listeners = getRunListeners(args);
		listeners.starting();
		try {
        

			ApplicationArguments applicationArguments = new DefaultApplicationArguments(args);
      //创建运行时环境
			ConfigurableEnvironment environment = prepareEnvironment(listeners, applicationArguments);
      //如果有配置 spring.beaninfo.ignore 参数，则将spring.beaninfo.ignore的值配置进去
			configureIgnoreBeanInfo(environment);
      //打印banner信息
			Banner printedBanner = printBanner(environment);
      //创建spring上下文
			context = createApplicationContext();
			exceptionReporters = getSpringFactoriesInstances(SpringBootExceptionReporter.class,
					new Class[] { ConfigurableApplicationContext.class }, context);
			prepareContext(context, environment, listeners, applicationArguments, printedBanner);
			refreshContext(context);
			afterRefresh(context, applicationArguments);
			stopWatch.stop();
			if (this.logStartupInfo) {
				new StartupInfoLogger(this.mainApplicationClass).logStarted(getApplicationLog(), stopWatch);
			}
			listeners.started(context);
			callRunners(context, applicationArguments);
		}
		catch (Throwable ex) {
			handleRunFailure(context, ex, exceptionReporters, listeners);
			throw new IllegalStateException(ex);
		}

		try {
			listeners.running(context);
		}
		catch (Throwable ex) {
			handleRunFailure(context, ex, exceptionReporters, null);
			throw new IllegalStateException(ex);
		}
		return context;
	}



private ConfigurableEnvironment prepareEnvironment(SpringApplicationRunListeners listeners,
			ApplicationArguments applicationArguments) {
		// Create and configure the environment
		ConfigurableEnvironment environment = getOrCreateEnvironment();
		configureEnvironment(environment, applicationArguments.getSourceArgs());
		ConfigurationPropertySources.attach(environment);
		listeners.environmentPrepared(environment);
		bindToSpringApplication(environment);
		if (!this.isCustomEnvironment) {
			environment = new EnvironmentConverter(getClassLoader()).convertEnvironmentIfNecessary(environment,
					deduceEnvironmentClass());
		}
		ConfigurationPropertySources.attach(environment);
		return environment;
	}
```



### createApplicationContext

```java
protected ConfigurableApplicationContext createApplicationContext() {
		Class<?> contextClass = this.applicationContextClass;
		if (contextClass == null) {
			try {
				switch (this.webApplicationType) {
				case SERVLET:
					contextClass = Class.forName(DEFAULT_SERVLET_WEB_CONTEXT_CLASS);
					break;
				case REACTIVE:
					contextClass = Class.forName(DEFAULT_REACTIVE_WEB_CONTEXT_CLASS);
					break;
				default:
					contextClass = Class.forName(DEFAULT_CONTEXT_CLASS);
				}
			}
			catch (ClassNotFoundException ex) {
				throw new IllegalStateException(
						"Unable create a default ApplicationContext, please specify an ApplicationContextClass", ex);
			}
		}
		return (ConfigurableApplicationContext) BeanUtils.instantiateClass(contextClass);
	}
```



### AnnotationConfigServletWebServerApplicationContext



```java
public static final String DEFAULT_SERVLET_WEB_CONTEXT_CLASS = "org.springframework.boot."
      + "web.servlet.context.AnnotationConfigServletWebServerApplicationContext";
```



#### 问题： 真正创建的BeanFactory在哪里？？？

##### 理清继承关系

```java
public class AnnotationConfigServletWebServerApplicationContext extends ServletWebServerApplicationContext
		implements AnnotationConfigRegistry {
//构造函数：
  public AnnotationConfigServletWebServerApplicationContext() {
		this.reader = new AnnotatedBeanDefinitionReader(this);
		this.scanner = new ClassPathBeanDefinitionScanner(this);
	}
  
  -----> 父类： ServletWebServerApplicationContext
       public class ServletWebServerApplicationContext extends GenericWebApplicationContext
		implements ConfigurableWebServerApplicationContext {
      构造函数： 
        
	       /**
	        * Create a new {@link ServletWebServerApplicationContext}.
	        */
	     public ServletWebServerApplicationContext() {
	      }
         
    ----->父类： 
      public class GenericWebApplicationContext extends GenericApplicationContext
		implements ConfigurableWebApplicationContext, ThemeSource {
       构造函数： 
        	public GenericWebApplicationContext() {
		      super();
	       }
    ------>父类： 
      public class GenericApplicationContext extends AbstractApplicationContext implements        BeanDefinitionRegistry {
        构造函数： 
        public GenericApplicationContext() {
        		this.beanFactory = new DefaultListableBeanFactory();
	      }

       注意： 到这里是真正的创建应用上下文  applicationContext；
---------------------------------------------------------------------------------------------------------------
         
       
    DefaultListableBeanFactory
 public class DefaultListableBeanFactory extends AbstractAutowireCapableBeanFactory
		implements ConfigurableListableBeanFactory, BeanDefinitionRegistry, Serializable {
     构造函数： 
     	public DefaultListableBeanFactory() {
		    super();
	    }
  
   -------->父类： AbstractAutowireCapableBeanFactory
   public abstract class AbstractAutowireCapableBeanFactory extends AbstractBeanFactory
		implements AutowireCapableBeanFactory {
       构造函数： 
         
	/**
	 * Create a new AbstractAutowireCapableBeanFactory.
	 */
	public AbstractAutowireCapableBeanFactory() {
		super();
		ignoreDependencyInterface(BeanNameAware.class);
		ignoreDependencyInterface(BeanFactoryAware.class);
		ignoreDependencyInterface(BeanClassLoaderAware.class);
	}
  
  ------->父类： AbstractBeanFactory
    public abstract class AbstractBeanFactory extends FactoryBeanRegistrySupport implements ConfigurableBeanFactory {
      构造函数： 
        public AbstractBeanFactory() {
	      }
      
   ------->父类：FactoryBeanRegistrySupport
     public abstract class FactoryBeanRegistrySupport extends DefaultSingletonBeanRegistry {

   -------->父类：DefaultSingletonBeanRegistry
     public class DefaultSingletonBeanRegistry extends SimpleAliasRegistry implements SingletonBeanRegistry {
    
     -------->父类： SimpleAliasRegistry
       public class SimpleAliasRegistry implements AliasRegistry {

-----接口层面：          AbstractBeanFactory  
  public abstract class AbstractBeanFactory extends FactoryBeanRegistrySupport implements ConfigurableBeanFactory {

     ----->父接口： 
         public interface ConfigurableBeanFactory extends HierarchicalBeanFactory, SingletonBeanRegistry {
       
     ----->父接口： 
       public interface HierarchicalBeanFactory extends BeanFactory {
   



     
         
```



### prepareContext()

```java
	private void prepareContext(ConfigurableApplicationContext context, ConfigurableEnvironment environment,
			SpringApplicationRunListeners listeners, ApplicationArguments applicationArguments, Banner printedBanner) {
    //设置environment到applicationConttext中 environment和applicationContext挂钩
		context.setEnvironment(environment);
    //
		postProcessApplicationContext(context);
    // 执行Initializer  public interface ApplicationContextInitializer<C extends ConfigurableApplicationContext> {
		applyInitializers(context);
    // SpringApplicationRunListeners的contextPrepared方法（在创建和准备ApplicationContext之后，但在加载之前）
		listeners.contextPrepared(context);
		if (this.logStartupInfo) {
			logStartupInfo(context.getParent() == null);
			logStartupProfileInfo(context);
		}
		// Add boot specific singleton beans
		ConfigurableListableBeanFactory beanFactory = context.getBeanFactory();
		beanFactory.registerSingleton("springApplicationArguments", applicationArguments);
		if (printedBanner != null) {
			beanFactory.registerSingleton("springBootBanner", printedBanner);
		}
		if (beanFactory instanceof DefaultListableBeanFactory) {
			((DefaultListableBeanFactory) beanFactory)
					.setAllowBeanDefinitionOverriding(this.allowBeanDefinitionOverriding);
		}
    //增加后置处理器
		if (this.lazyInitialization) {
			context.addBeanFactoryPostProcessor(new LazyInitializationBeanFactoryPostProcessor());
		}
		// Load the sources  加载主启动类
		Set<Object> sources = getAllSources();
		Assert.notEmpty(sources, "Sources must not be empty");
    //注册主启动类
		load(context, sources.toArray(new Object[0]));
    //发送监听事件，上下文开始加载，真正的上下文还没有加载，只是简单初始化进行了一些处理
		listeners.contextLoaded(context);
	}

```



#### postProcessApplicationContext(context);

```java
protected void postProcessApplicationContext(ConfigurableApplicationContext context) {
		if (this.beanNameGenerator != null) {
			context.getBeanFactory().registerSingleton(AnnotationConfigUtils.CONFIGURATION_BEAN_NAME_GENERATOR,
					this.beanNameGenerator);
		}
		if (this.resourceLoader != null) {
			if (context instanceof GenericApplicationContext) {
				((GenericApplicationContext) context).setResourceLoader(this.resourceLoader);
			}
			if (context instanceof DefaultResourceLoader) {
				((DefaultResourceLoader) context).setClassLoader(this.resourceLoader.getClassLoader());
			}
		}
		if (this.addConversionService) {
			context.getBeanFactory().setConversionService(ApplicationConversionService.getSharedInstance());
		}
	}
```





#### applyInitializers(context);

![image-20220215140814930](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215140814930.png)



#### 打印启动信息

```java
if (this.logStartupInfo) {
			logStartupInfo(context.getParent() == null);
			logStartupProfileInfo(context);
		}
```



```shell
2022-02-15 14:10:58.924  INFO 41225 --- [           main] com.xs.micro.boot.MicroBootApplication   : Starting MicroBootApplication on topwqpdeMacBook-Pro.local with PID 41225 (/Users/topwqp/Documents/work/projects/xiangshang/micro-spring-boot/target/classes started by topwqp in /Users/topwqp/Documents/work/projects/xiangshang/micro-spring-boot)
2022-02-15 14:11:07.130  INFO 41225 --- [           main] com.xs.micro.boot.MicroBootApplication   : No active profile set, falling back to default profiles: default

```

####  getAllSources()

加载主启动类：

![image-20220215141547524](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215141547524.png)

#### load

```java
protected void load(ApplicationContext context, Object[] sources) {
		if (logger.isDebugEnabled()) {
			logger.debug("Loading source " + StringUtils.arrayToCommaDelimitedString(sources));
		}
    //创建各种reader，比如： AnnotatedBeanDefinitionReader 和 XmlBeanDefinitionReader
		BeanDefinitionLoader loader = createBeanDefinitionLoader(getBeanDefinitionRegistry(context), sources);
   // 设置BeanName生成器，目前还未注册
		if (this.beanNameGenerator != null) {
      //作用是如何生成bean的name，首字母小写这种。。。
			loader.setBeanNameGenerator(this.beanNameGenerator);
		}
		if (this.resourceLoader != null) {
			loader.setResourceLoader(this.resourceLoader);
		}
		if (this.environment != null) {
			loader.setEnvironment(this.environment);
		}
		loader.load();
	}
```



#### createBeanDefinitionLoader

**org.springframework.boot.BeanDefinitionLoader#BeanDefinitionLoader**

加载BeanDefinitionReader

```java
	BeanDefinitionLoader(BeanDefinitionRegistry registry, Object... sources) {
		Assert.notNull(registry, "Registry must not be null");
		Assert.notEmpty(sources, "Sources must not be empty");
		this.sources = sources;
		this.annotatedReader = new AnnotatedBeanDefinitionReader(registry);
		this.xmlReader = new XmlBeanDefinitionReader(registry);
		if (isGroovyPresent()) {
			this.groovyReader = new GroovyBeanDefinitionReader(registry);
		}
		this.scanner = new ClassPathBeanDefinitionScanner(registry);
		this.scanner.addExcludeFilter(new ClassExcludeFilter(sources));
	}


```

![image-20220215142210804](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215142210804.png)



开始加载注解bean等： 

`org.springframework.context.annotation.AnnotationConfigUtils#processCommonDefinitionAnnotations(org.springframework.beans.factory.annotation.AnnotatedBeanDefinition, org.springframework.core.type.AnnotatedTypeMetadata)`



## Spring refresh

#### @SpringBootApplication注解是什么时候被扫描处理的？？？

![image-20220210180758077](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220210180758077.png)



#### getBeanDefinitionRegistry

这个`BeanDefinitionRegistry`的获取直接用 强转applicationContext，因为`org.springframework.boot.web.servlet.context.AnnotationConfigServletWebServerApplicationContext`往上找映射关系，最终会是 `BeanDefinitionRegistry`的实现

```java
private BeanDefinitionRegistry getBeanDefinitionRegistry(ApplicationContext context) {
		if (context instanceof BeanDefinitionRegistry) {
			return (BeanDefinitionRegistry) context;
		}
		if (context instanceof AbstractApplicationContext) {
			return (BeanDefinitionRegistry) ((AbstractApplicationContext) context).getBeanFactory();
		}
		throw new IllegalStateException("Could not locate BeanDefinitionRegistry");
	}
```



#### BeanDefinitionLoader 说明

就是加载各种资源的一个超级加载器

代码说明： 是`AnnotatedBeanDefinitionReader` 和 `XmlBeanDefinitionReader`以及`BeanDefinitionReader`等的持有者，主要是为了方便后期加载读取 注解、xml以及javaConfig的相关配置

```java
/**
 * Loads bean definitions from underlying sources, including XML and JavaConfig. Acts as a
 * simple facade over {@link AnnotatedBeanDefinitionReader},
 * {@link XmlBeanDefinitionReader} and {@link ClassPathBeanDefinitionScanner}. See
 * {@link SpringApplication} for the types of sources that are supported.
 *
 * @author Phillip Webb
 * @author Vladislav Kisel
 * @see #setBeanNameGenerator(BeanNameGenerator)
 */
class BeanDefinitionLoader {

	private final Object[] sources;

	private final AnnotatedBeanDefinitionReader annotatedReader;

	private final XmlBeanDefinitionReader xmlReader;

	private BeanDefinitionReader groovyReader;

	private final ClassPathBeanDefinitionScanner scanner;

	private ResourceLoader resourceLoader;

	/**
	 * Create a new {@link BeanDefinitionLoader} that will load beans into the specified
	 * {@link BeanDefinitionRegistry}.
	 * @param registry the bean definition registry that will contain the loaded beans
	 * @param sources the bean sources
	 */
	BeanDefinitionLoader(BeanDefinitionRegistry registry, Object... sources) {
		Assert.notNull(registry, "Registry must not be null");
		Assert.notEmpty(sources, "Sources must not be empty");
		this.sources = sources;
		this.annotatedReader = new AnnotatedBeanDefinitionReader(registry);
		this.xmlReader = new XmlBeanDefinitionReader(registry);
		if (isGroovyPresent()) {
			this.groovyReader = new GroovyBeanDefinitionReader(registry);
		}
		this.scanner = new ClassPathBeanDefinitionScanner(registry);
		this.scanner.addExcludeFilter(new ClassExcludeFilter(sources));
	}
```



#### loader.load()



![image-20220215143637028](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215143637028.png)



```java
	private int load(Class<?> source) {
		if (isGroovyPresent() && GroovyBeanDefinitionSource.class.isAssignableFrom(source)) {
			// Any GroovyLoaders added in beans{} DSL can contribute beans here
			GroovyBeanDefinitionSource loader = BeanUtils.instantiateClass(source, GroovyBeanDefinitionSource.class);
			load(loader);
		}
		if (isEligible(source)) {
			this.annotatedReader.register(source);
			return 1;
		}
		return 0;
	}

```



##### isEligible

检查这个bean是否有资格注册，不是groovy并且不是匿名类并且有构造函数就可以注册

```java
/**
	 * Check whether the bean is eligible for registration.
	 * @param type candidate bean type
	 * @return true if the given bean type is eligible for registration, i.e. not a groovy
	 * closure nor an anonymous class
	 */
	private boolean isEligible(Class<?> type) {
		return !(type.isAnonymousClass() || isGroovyClosure(type) || hasNoConstructors(type));
	}
```



##### register

```java
/**
	 * Register one or more component classes to be processed.
	 * <p>Calls to {@code register} are idempotent; adding the same
	 * component class more than once has no additional effect.
	 * @param componentClasses one or more component classes,
	 * e.g. {@link Configuration @Configuration} classes
	 */
	public void register(Class<?>... componentClasses) {
		for (Class<?> componentClass : componentClasses) {
			registerBean(componentClass);
		}
	}
```



#### doRegisterBean 

读过spring源码的都知道，真正做事情的方法，都会再前面加上一个doXXX，这种操作

```java
/**
	 * Register a bean from the given bean class, deriving its metadata from
	 * class-declared annotations.
	 * @param beanClass the class of the bean
	 * @param name an explicit name for the bean
	 * @param qualifiers specific qualifier annotations to consider, if any,
	 * in addition to qualifiers at the bean class level
	 * @param supplier a callback for creating an instance of the bean
	 * (may be {@code null})
	 * @param customizers one or more callbacks for customizing the factory's
	 * {@link BeanDefinition}, e.g. setting a lazy-init or primary flag
	 * @since 5.0
	 */
	private <T> void doRegisterBean(Class<T> beanClass, @Nullable String name,
			@Nullable Class<? extends Annotation>[] qualifiers, @Nullable Supplier<T> supplier,
			@Nullable BeanDefinitionCustomizer[] customizers) {

    //加载一个类从加载class com.xs.micro.boot.MicroBootApplication开始，把class转变为对应的BeanDefinition
		AnnotatedGenericBeanDefinition abd = new AnnotatedGenericBeanDefinition(beanClass);
		if (this.conditionEvaluator.shouldSkip(abd.getMetadata())) {
			return;
		}

		abd.setInstanceSupplier(supplier);
    //解析bean的scope
		ScopeMetadata scopeMetadata = this.scopeMetadataResolver.resolveScopeMetadata(abd);
		abd.setScope(scopeMetadata.getScopeName());
    //生成bean的名称
		String beanName = (name != null ? name : this.beanNameGenerator.generateBeanName(abd, this.registry));
    //解析BeanDefinition的注解
		AnnotationConfigUtils.processCommonDefinitionAnnotations(abd);
		if (qualifiers != null) {
			for (Class<? extends Annotation> qualifier : qualifiers) {
				if (Primary.class == qualifier) {
					abd.setPrimary(true);
				}
				else if (Lazy.class == qualifier) {
					abd.setLazyInit(true);
				}
				else {
					abd.addQualifier(new AutowireCandidateQualifier(qualifier));
				}
			}
		}
		if (customizers != null) {
			for (BeanDefinitionCustomizer customizer : customizers) {
				customizer.customize(abd);
			}
		}
    
    //包装成BeanDefinitionHolder，将BeanDefinition注册到IOC容器中
		BeanDefinitionHolder definitionHolder = new BeanDefinitionHolder(abd, beanName);
		definitionHolder = AnnotationConfigUtils.applyScopedProxyMode(scopeMetadata, definitionHolder, this.registry);
		BeanDefinitionReaderUtils.registerBeanDefinition(definitionHolder, this.registry);
	}
```



#### 扩展

这里定义风格和之前的applicationContext 以及前面的environment类的组织关系类似

```java
public abstract class AbstractBeanDefinition extends BeanMetadataAttributeAccessor
		implements BeanDefinition, Cloneable {

public class GenericBeanDefinition extends AbstractBeanDefinition {

public class AnnotatedGenericBeanDefinition extends GenericBeanDefinition implements AnnotatedBeanDefinition {
  
```



#### processCommonDefinitionAnnotations

解析对应的注解，比如@lazy @role  @primary  @description等

```java
static void processCommonDefinitionAnnotations(AnnotatedBeanDefinition abd, AnnotatedTypeMetadata metadata) {
		AnnotationAttributes lazy = attributesFor(metadata, Lazy.class);
		if (lazy != null) {
			abd.setLazyInit(lazy.getBoolean("value"));
		}
		else if (abd.getMetadata() != metadata) {
			lazy = attributesFor(abd.getMetadata(), Lazy.class);
			if (lazy != null) {
				abd.setLazyInit(lazy.getBoolean("value"));
			}
		}

		if (metadata.isAnnotated(Primary.class.getName())) {
			abd.setPrimary(true);
		}
		AnnotationAttributes dependsOn = attributesFor(metadata, DependsOn.class);
		if (dependsOn != null) {
			abd.setDependsOn(dependsOn.getStringArray("value"));
		}

		AnnotationAttributes role = attributesFor(metadata, Role.class);
		if (role != null) {
			abd.setRole(role.getNumber("value").intValue());
		}
		AnnotationAttributes description = attributesFor(metadata, Description.class);
		if (description != null) {
			abd.setDescription(description.getString("value"));
		}
	}
```



#### BeanDefinitionReaderUtils.registerBeanDefinition

把对应的BeanDefinition注册到ioc容器中

```java
/**
	 * Register the given bean definition with the given bean factory.
	 * @param definitionHolder the bean definition including name and aliases
	 * @param registry the bean factory to register with
	 * @throws BeanDefinitionStoreException if registration failed
	 */
	public static void registerBeanDefinition(
			BeanDefinitionHolder definitionHolder, BeanDefinitionRegistry registry)
			throws BeanDefinitionStoreException {

		// Register bean definition under primary name.
		String beanName = definitionHolder.getBeanName();
    //
		registry.registerBeanDefinition(beanName, definitionHolder.getBeanDefinition());

		// Register aliases for bean name, if any.
		String[] aliases = definitionHolder.getAliases();
		if (aliases != null) {
			for (String alias : aliases) {
				registry.registerAlias(beanName, alias);
			}
		}
	}
```

#### registerBeanDefinition

```java
@Override
	public void registerBeanDefinition(String beanName, BeanDefinition beanDefinition)
			throws BeanDefinitionStoreException {

		this.beanFactory.registerBeanDefinition(beanName, beanDefinition);
	}

```



#### DefaultListableBeanFactory.beanDefinitionMap中

```java
private final Map<String, BeanDefinition> beanDefinitionMap = new ConcurrentHashMap<>(256);

@Override
	public void registerBeanDefinition(String beanName, BeanDefinition beanDefinition)
			throws BeanDefinitionStoreException {

		Assert.hasText(beanName, "Bean name must not be empty");
		Assert.notNull(beanDefinition, "BeanDefinition must not be null");

		if (beanDefinition instanceof AbstractBeanDefinition) {
			try {
				((AbstractBeanDefinition) beanDefinition).validate();
			}
			catch (BeanDefinitionValidationException ex) {
				throw new BeanDefinitionStoreException(beanDefinition.getResourceDescription(), beanName,
						"Validation of bean definition failed", ex);
			}
		}

		BeanDefinition existingDefinition = this.beanDefinitionMap.get(beanName);
		if (existingDefinition != null) {
			if (!isAllowBeanDefinitionOverriding()) {
				throw new BeanDefinitionOverrideException(beanName, beanDefinition, existingDefinition);
			}
			else if (existingDefinition.getRole() < beanDefinition.getRole()) {
				// e.g. was ROLE_APPLICATION, now overriding with ROLE_SUPPORT or ROLE_INFRASTRUCTURE
				if (logger.isInfoEnabled()) {
					logger.info("Overriding user-defined bean definition for bean '" + beanName +
							"' with a framework-generated bean definition: replacing [" +
							existingDefinition + "] with [" + beanDefinition + "]");
				}
			}
			else if (!beanDefinition.equals(existingDefinition)) {
				if (logger.isDebugEnabled()) {
					logger.debug("Overriding bean definition for bean '" + beanName +
							"' with a different definition: replacing [" + existingDefinition +
							"] with [" + beanDefinition + "]");
				}
			}
			else {
				if (logger.isTraceEnabled()) {
					logger.trace("Overriding bean definition for bean '" + beanName +
							"' with an equivalent definition: replacing [" + existingDefinition +
							"] with [" + beanDefinition + "]");
				}
			}
			this.beanDefinitionMap.put(beanName, beanDefinition);
		}
		else {
			if (hasBeanCreationStarted()) {
				// Cannot modify startup-time collection elements anymore (for stable iteration)
				synchronized (this.beanDefinitionMap) {
					this.beanDefinitionMap.put(beanName, beanDefinition);
					List<String> updatedDefinitions = new ArrayList<>(this.beanDefinitionNames.size() + 1);
					updatedDefinitions.addAll(this.beanDefinitionNames);
					updatedDefinitions.add(beanName);
					this.beanDefinitionNames = updatedDefinitions;
					removeManualSingletonName(beanName);
				}
			}
			else {
				// Still in startup registration phase
				this.beanDefinitionMap.put(beanName, beanDefinition);
				this.beanDefinitionNames.add(beanName);
				removeManualSingletonName(beanName);
			}
			this.frozenBeanDefinitionNames = null;
		}

		if (existingDefinition != null || containsSingleton(beanName)) {
			resetBeanDefinition(beanName);
		}
		else if (isConfigurationFrozen()) {
			clearByTypeCache();
		}
	}
```

#### listeners.contextLoaded(context);

![image-20220215152305646](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215152305646.png)



### refreshContext



#### refreshContext

```java
private void refreshContext(ConfigurableApplicationContext context) {
   refresh((ApplicationContext) context);
   if (this.registerShutdownHook) {
      try {
        //  向JVM运行时注册一个shutdown的钩子，这个钩子的作用是监听JVM关闭时销毁IOC容器和里面的Bean
         context.registerShutdownHook();
      }
      catch (AccessControlException ex) {
         // Not allowed in some environments.
      }
   }
}
```

#### 钩子registerShutdownHook

```java
@Override
	public void registerShutdownHook() {
		if (this.shutdownHook == null) {
			// No shutdown hook registered yet.
			this.shutdownHook = new Thread(SHUTDOWN_HOOK_THREAD_NAME) {
				@Override
				public void run() {
					synchronized (startupShutdownMonitor) {
						doClose();
					}
				}
			};
			Runtime.getRuntime().addShutdownHook(this.shutdownHook);
		}
	}
```

最终调用的是AbstractApplicationContext.doClose()方法

```java
	/**
	 * Actually performs context closing: publishes a ContextClosedEvent and
	 * destroys the singletons in the bean factory of this application context.
	 * <p>Called by both {@code close()} and a JVM shutdown hook, if any.
	 * @see org.springframework.context.event.ContextClosedEvent
	 * @see #destroyBeans()
	 * @see #close()
	 * @see #registerShutdownHook()
	 */
	protected void doClose() {
		// Check whether an actual close attempt is necessary...
		if (this.active.get() && this.closed.compareAndSet(false, true)) {
			if (logger.isDebugEnabled()) {
				logger.debug("Closing " + this);
			}

			LiveBeansView.unregisterApplicationContext(this);

			try {
				// Publish shutdown event.
				publishEvent(new ContextClosedEvent(this));
			}
			catch (Throwable ex) {
				logger.warn("Exception thrown from ApplicationListener handling ContextClosedEvent", ex);
			}

			// Stop all Lifecycle beans, to avoid delays during individual destruction.
			if (this.lifecycleProcessor != null) {
				try {
					this.lifecycleProcessor.onClose();
				}
				catch (Throwable ex) {
					logger.warn("Exception thrown from LifecycleProcessor on context close", ex);
				}
			}

			// Destroy all cached singletons in the context's BeanFactory.
			destroyBeans();

			// Close the state of this context itself.
			closeBeanFactory();

			// Let subclasses do some final clean-up if they wish...
			onClose();

			// Reset local application listeners to pre-refresh state.
			if (this.earlyApplicationListeners != null) {
				this.applicationListeners.clear();
				this.applicationListeners.addAll(this.earlyApplicationListeners);
			}

			// Switch to inactive.
			this.active.set(false);
		}
	}
```



### ServletWebServerApplicationContext.refresh

`org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext#refresh`

```java
	@Override
	public final void refresh() throws BeansException, IllegalStateException {
		try {
			super.refresh();
		}
		catch (RuntimeException ex) {
			WebServer webServer = this.webServer;
			if (webServer != null) {
				webServer.stop();
			}
			throw ex;
		}
	}
```

第一句就是直接`super.refresh`方法，这个



**以下由`SpringApplication`中的`refresh`方法调用到 `AbstractApplicationContext`的 `refresh`**



#### AbstractApplicationContext.refresh()方法

以下是`AbstractApplicationContext`的`refresh`方法

## refresh

```java
@Override
	public void refresh() throws BeansException, IllegalStateException {
		synchronized (this.startupShutdownMonitor) {
			// Prepare this context for refreshing.   
			prepareRefresh();

			// Tell the subclass to refresh the internal bean factory.
			ConfigurableListableBeanFactory beanFactory = obtainFreshBeanFactory();

			// Prepare the bean factory for use in this context.
			prepareBeanFactory(beanFactory);

			try {
				// Allows post-processing of the bean factory in context subclasses.
				postProcessBeanFactory(beanFactory);

				// Invoke factory processors registered as beans in the context.
				invokeBeanFactoryPostProcessors(beanFactory);

				// Register bean processors that intercept bean creation.
				registerBeanPostProcessors(beanFactory);

				// Initialize message source for this context.
				initMessageSource();

				// Initialize event multicaster for this context.
				initApplicationEventMulticaster();

				// Initialize other special beans in specific context subclasses.
				onRefresh();

				// Check for listener beans and register them.
				registerListeners();

				// Instantiate all remaining (non-lazy-init) singletons.
				finishBeanFactoryInitialization(beanFactory);

				// Last step: publish corresponding event.
				finishRefresh();
			}

			catch (BeansException ex) {
				if (logger.isWarnEnabled()) {
					logger.warn("Exception encountered during context initialization - " +
							"cancelling refresh attempt: " + ex);
				}

				// Destroy already created singletons to avoid dangling resources.
				destroyBeans();

				// Reset 'active' flag.
				cancelRefresh(ex);

				// Propagate exception to caller.
				throw ex;
			}

			finally {
				// Reset common introspection caches in Spring's core, since we
				// might not ever need metadata for singleton beans anymore...
				resetCommonCaches();
			}
		}
	}

```



**以下分步骤讲解这些核心方法，不管是`spring boot` 还是 `spring `这个都是核心**

### prepareRefresh

```java
/**
	 * Prepare this context for refreshing, setting its startup date and
	 * active flag as well as performing any initialization of property sources.
	 */
	protected void prepareRefresh() {
		// Switch to active.
		this.startupDate = System.currentTimeMillis();
    //标记IOC容器的关闭状态为false
		this.closed.set(false);
    //标记IOC容器已激活
		this.active.set(true);

		if (logger.isDebugEnabled()) {
			if (logger.isTraceEnabled()) {
				logger.trace("Refreshing " + this);
			}
			else {
				logger.debug("Refreshing " + getDisplayName());
			}
		}

		// Initialize any placeholder property sources in the context environment.
    //初始化属性配置
		initPropertySources();

		// Validate that all properties marked as required are resolvable:
		// see ConfigurablePropertyResolver#setRequiredProperties
    //校验环境配置信息
		getEnvironment().validateRequiredProperties();

		// Store pre-refresh ApplicationListeners...
		if (this.earlyApplicationListeners == null) {
			this.earlyApplicationListeners = new LinkedHashSet<>(this.applicationListeners);
		}
		else {
			// Reset local application listeners to pre-refresh state.
			this.applicationListeners.clear();
			this.applicationListeners.addAll(this.earlyApplicationListeners);
		}

		// Allow for the collection of early ApplicationEvents,
		// to be published once the multicaster is available...
		this.earlyApplicationEvents = new LinkedHashSet<>();
	}
```



#### initPropertySources

`org.springframework.web.context.support.GenericWebApplicationContext#initPropertySources`

```java
@Override
	protected void initPropertySources() {
		ConfigurableEnvironment env = getEnvironment();
		if (env instanceof ConfigurableWebEnvironment) {
			((ConfigurableWebEnvironment) env).initPropertySources(this.servletContext, null);
		}
	}
```



#### StandardServletEnvironment.initPropertySources

`org.springframework.web.context.support.StandardServletEnvironment#initPropertySources`

```java
@Override
	public void initPropertySources(@Nullable ServletContext servletContext, @Nullable ServletConfig servletConfig) {
		WebApplicationContextUtils.initServletPropertySources(getPropertySources(), servletContext, servletConfig);
	}
```



做一些servlet上下文参数初始化，比如`web.xml`相关配置的参数，但是我这里debug了，没有设定

```java
public static void initServletPropertySources(MutablePropertySources sources,
			@Nullable ServletContext servletContext, @Nullable ServletConfig servletConfig) {

		Assert.notNull(sources, "'propertySources' must not be null");
		String name = StandardServletEnvironment.SERVLET_CONTEXT_PROPERTY_SOURCE_NAME;
		if (servletContext != null && sources.get(name) instanceof StubPropertySource) {
			sources.replace(name, new ServletContextPropertySource(name, servletContext));
		}
		name = StandardServletEnvironment.SERVLET_CONFIG_PROPERTY_SOURCE_NAME;
		if (servletConfig != null && sources.get(name) instanceof StubPropertySource) {
			sources.replace(name, new ServletConfigPropertySource(name, servletConfig));
		}
	}
```



![image-20220215161053322](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215161053322.png)



#### getEnvironment().validateRequiredProperties();

方法描述用于是否校验属性，但是debug后，无校验

![image-20220215161330029](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215161330029.png)



#### Store pre-refresh ApplicationListeners

![image-20220215161438937](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215161438937.png)



### obtainFreshBeanFactory

```java
protected ConfigurableListableBeanFactory obtainFreshBeanFactory() {
		refreshBeanFactory();
		return getBeanFactory();
	}
```



#### org.springframework.context.support.GenericApplicationContext#refreshBeanFactory

只是设置了一下上下文ID

```java
@Override
	protected final void refreshBeanFactory() throws IllegalStateException {
		if (!this.refreshed.compareAndSet(false, true)) {
			throw new IllegalStateException(
					"GenericApplicationContext does not support multiple refresh attempts: just call 'refresh' once");
		}
		this.beanFactory.setSerializationId(getId());
	}

```

### prepareBeanFactory(beanFactory)



```java
/**
	 * Configure the factory's standard context characteristics,
	 * such as the context's ClassLoader and post-processors.
	 * @param beanFactory the BeanFactory to configure
	 */
	protected void prepareBeanFactory(ConfigurableListableBeanFactory beanFactory) {
		// Tell the internal bean factory to use the context's class loader etc.
		beanFactory.setBeanClassLoader(getClassLoader());
		beanFactory.setBeanExpressionResolver(new StandardBeanExpressionResolver(beanFactory.getBeanClassLoader()));
		beanFactory.addPropertyEditorRegistrar(new ResourceEditorRegistrar(this, getEnvironment()));

		// Configure the bean factory with context callbacks.
		beanFactory.addBeanPostProcessor(new ApplicationContextAwareProcessor(this));
		beanFactory.ignoreDependencyInterface(EnvironmentAware.class);
		beanFactory.ignoreDependencyInterface(EmbeddedValueResolverAware.class);
		beanFactory.ignoreDependencyInterface(ResourceLoaderAware.class);
		beanFactory.ignoreDependencyInterface(ApplicationEventPublisherAware.class);
		beanFactory.ignoreDependencyInterface(MessageSourceAware.class);
		beanFactory.ignoreDependencyInterface(ApplicationContextAware.class);

		// BeanFactory interface not registered as resolvable type in a plain factory.
		// MessageSource registered (and found for autowiring) as a bean.
		beanFactory.registerResolvableDependency(BeanFactory.class, beanFactory);
		beanFactory.registerResolvableDependency(ResourceLoader.class, this);
		beanFactory.registerResolvableDependency(ApplicationEventPublisher.class, this);
		beanFactory.registerResolvableDependency(ApplicationContext.class, this);

		// Register early post-processor for detecting inner beans as ApplicationListeners.
		beanFactory.addBeanPostProcessor(new ApplicationListenerDetector(this));

		// Detect a LoadTimeWeaver and prepare for weaving, if found.
		if (beanFactory.containsBean(LOAD_TIME_WEAVER_BEAN_NAME)) {
			beanFactory.addBeanPostProcessor(new LoadTimeWeaverAwareProcessor(beanFactory));
			// Set a temporary ClassLoader for type matching.
			beanFactory.setTempClassLoader(new ContextTypeMatchClassLoader(beanFactory.getBeanClassLoader()));
		}

		// Register default environment beans.
		if (!beanFactory.containsLocalBean(ENVIRONMENT_BEAN_NAME)) {
			beanFactory.registerSingleton(ENVIRONMENT_BEAN_NAME, getEnvironment());
		}
		if (!beanFactory.containsLocalBean(SYSTEM_PROPERTIES_BEAN_NAME)) {
			beanFactory.registerSingleton(SYSTEM_PROPERTIES_BEAN_NAME, getEnvironment().getSystemProperties());
		}
		if (!beanFactory.containsLocalBean(SYSTEM_ENVIRONMENT_BEAN_NAME)) {
			beanFactory.registerSingleton(SYSTEM_ENVIRONMENT_BEAN_NAME, getEnvironment().getSystemEnvironment());
		}
	}

	/**
	 * Modify the application context's internal bean factory after its standard
	 * initialization. All bean definitions will have been loaded, but no beans
	 * will have been instantiated yet. This allows for registering special
	 * BeanPostProcessors etc in certain ApplicationContext implementations.
	 * @param beanFactory the bean factory used by the application context
	 */
	protected void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) {
	}

```



### BeanPostProcessor

这里需要了解下 `@PostConstruct`以及实现`InitializingBean`的`afterPropertiesSet`和 实现了 `BeanPostProcessor `的一个bean初始化时的顺序问题，之前讲过，不再讲述



### ApplicationContextAwareProcessor

主要作用，因为之前了解过 `BeanPostProcessor`，每个bean初始化完成都会调用，所以这个作用就是相当于把初始化的bean中含有实现了： `EnvironmentAware`,`EmbeddedValueResolverAware`,`ResourceLoaderAware`,`ApplicationEventPublisherAware`,`MessageSourceAware` `ApplicationContextAware`，均注入一遍，aware本身在spring的设计中，是持有者的概念，所以本身是把相关的 环境信息和应用信息`applicationContext`注入到对应的 bean中，相当于bean有了这些功能

```java
beanFactory.addBeanPostProcessor(new ApplicationContextAwareProcessor(this));

@Override
	@Nullable
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		if (!(bean instanceof EnvironmentAware || bean instanceof EmbeddedValueResolverAware ||
				bean instanceof ResourceLoaderAware || bean instanceof ApplicationEventPublisherAware ||
				bean instanceof MessageSourceAware || bean instanceof ApplicationContextAware)){
			return bean;
		}

		AccessControlContext acc = null;

		if (System.getSecurityManager() != null) {
			acc = this.applicationContext.getBeanFactory().getAccessControlContext();
		}

		if (acc != null) {
			AccessController.doPrivileged((PrivilegedAction<Object>) () -> {
				invokeAwareInterfaces(bean);
				return null;
			}, acc);
		}
		else {
			invokeAwareInterfaces(bean);
		}

		return bean;
	}

	private void invokeAwareInterfaces(Object bean) {
		if (bean instanceof EnvironmentAware) {
			((EnvironmentAware) bean).setEnvironment(this.applicationContext.getEnvironment());
		}
		if (bean instanceof EmbeddedValueResolverAware) {
			((EmbeddedValueResolverAware) bean).setEmbeddedValueResolver(this.embeddedValueResolver);
		}
		if (bean instanceof ResourceLoaderAware) {
			((ResourceLoaderAware) bean).setResourceLoader(this.applicationContext);
		}
		if (bean instanceof ApplicationEventPublisherAware) {
			((ApplicationEventPublisherAware) bean).setApplicationEventPublisher(this.applicationContext);
		}
		if (bean instanceof MessageSourceAware) {
			((MessageSourceAware) bean).setMessageSource(this.applicationContext);
		}
		if (bean instanceof ApplicationContextAware) {
			((ApplicationContextAware) bean).setApplicationContext(this.applicationContext);
		}
	}
```



这段代码的意义是，标记忽略一些注入： 

```java
	  beanFactory.ignoreDependencyInterface(EnvironmentAware.class);
		beanFactory.ignoreDependencyInterface(EmbeddedValueResolverAware.class);
		beanFactory.ignoreDependencyInterface(ResourceLoaderAware.class);
		beanFactory.ignoreDependencyInterface(ApplicationEventPublisherAware.class);
		beanFactory.ignoreDependencyInterface(MessageSourceAware.class);
		beanFactory.ignoreDependencyInterface(ApplicationContextAware.class);

标记以下这些不会生效： 
@Autowire 
ResourceLoaderAware aware;
@Autowire
EnvironmentAware envAwre;

```



[参考说明](https://blog.csdn.net/yuge1123/article/details/106053857) 

`BeanFactory`  `ResourceLoader`, `ApplicationEventPublisher`,`ApplicationContext` 这几个接口实现比较多，用的比较多，提前注入到容器中

```java
// BeanFactory interface not registered as resolvable type in a plain factory.
		// MessageSource registered (and found for autowiring) as a bean.
		beanFactory.registerResolvableDependency(BeanFactory.class, beanFactory);
		beanFactory.registerResolvableDependency(ResourceLoader.class, this);
		beanFactory.registerResolvableDependency(ApplicationEventPublisher.class, this);
		beanFactory.registerResolvableDependency(ApplicationContext.class, this);

```



### ApplicationListenerDetector

如果Bean是 `ApplicationListener` 的实现类，并且是单实例Bean，则会注册到IOC容器中

```java
@Override
	public void postProcessMergedBeanDefinition(RootBeanDefinition beanDefinition, Class<?> beanType, String beanName) {
		if (ApplicationListener.class.isAssignableFrom(beanType)) {
			this.singletonNames.put(beanName, beanDefinition.isSingleton());
		}
	}

	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) {
		return bean;
	}

	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) {
		if (bean instanceof ApplicationListener) {
			// potentially not detected as a listener by getBeanNamesForType retrieval
			Boolean flag = this.singletonNames.get(beanName);
			if (Boolean.TRUE.equals(flag)) {
				// singleton bean (top-level or inner): register on the fly
				this.applicationContext.addApplicationListener((ApplicationListener<?>) bean);
			}
			else if (Boolean.FALSE.equals(flag)) {
				if (logger.isWarnEnabled() && !this.applicationContext.containsBean(beanName)) {
					// inner bean with other scope - can't reliably process events
					logger.warn("Inner bean '" + beanName + "' implements ApplicationListener interface " +
							"but is not reachable for event multicasting by its containing ApplicationContext " +
							"because it does not have singleton scope. Only top-level listener beans are allowed " +
							"to be of non-singleton scope.");
				}
				this.singletonNames.remove(beanName);
			}
		}
		return bean;
	}

```



### postProcessBeanFactory(beanFactory);

`org.springframework.boot.web.servlet.context.AnnotationConfigServletWebServerApplicationContext#postProcessBeanFactory`



```java
@Override
	protected void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) {
		super.postProcessBeanFactory(beanFactory);
		if (this.basePackages != null && this.basePackages.length > 0) {
			this.scanner.scan(this.basePackages);
		}
		if (!this.annotatedClasses.isEmpty()) {
			this.reader.register(ClassUtils.toClassArray(this.annotatedClasses));
		}
	}
```





---> 父类的首先调用`postProcessBeanFactory`

`org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext#postProcessBeanFactory`

```java
@Override
	protected void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) {
		beanFactory.addBeanPostProcessor(new WebApplicationContextServletContextAwareProcessor(this));
		beanFactory.ignoreDependencyInterface(ServletContextAware.class);
		registerWebApplicationScopes();
	}
```



### WebApplicationContextServletContextAwareProcessor

`org.springframework.web.context.support.ServletContextAwareProcessor#postProcessBeforeInitialization`



目的是把实现了 `servletContextAware` 和 实现了 `ServletConfigAware` 两个接口的bean，通过调用`set`方法注入 `servletContext` 和 `servletConfig `到对应的bean中

```java
@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		if (getServletContext() != null && bean instanceof ServletContextAware) {
			((ServletContextAware) bean).setServletContext(getServletContext());
		}
		if (getServletConfig() != null && bean instanceof ServletConfigAware) {
			((ServletConfigAware) bean).setServletConfig(getServletConfig());
		}
		return bean;
	}
```



#### 扩展 xxxxAware

实现了 `xxxxxAware` 这种接口的bean代表某个bean持有某个对象，比如实现了 `ApplicationContextAware`接口的bean，具有获取applicationContext的作用，是在什么情况下，给对应的 bean注入`ApplicationContextAware`的呢？ 就是通过`BeanPostProcessor`的这种机制

比如：

通过：  `WebApplicationContextServletContextAwareProcessor`  注入 `ServletContext` 和 `ServletConfig`

通过： `ApplicationContextAwareProcessor` 注入实现了： `EnvironmentAware`  `EmbeddedValueResolverAware` 

 `ResourceLoaderAware`  `ApplicationEventPublisherAware`   `MessageSourceAware` `ApplicationContextAware`  接口注入对应的XXXXXX

#### registerWebApplicationScopes()

`org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext#registerWebApplicationScopes`

```java
private void registerWebApplicationScopes() {
    //注册reqeust 和 session的作用域
		ExistingWebApplicationScopes existingScopes = new ExistingWebApplicationScopes(getBeanFactory());
		WebApplicationContextUtils.registerWebApplicationScopes(getBeanFactory());
		existingScopes.restore();
	}
```

##### WebApplicationContextUtils.registerWebApplicationScopes

```java
public static void registerWebApplicationScopes(ConfigurableListableBeanFactory beanFactory,
			@Nullable ServletContext sc) {
  
    //将web的几个作用于注册到applicationContext中
		beanFactory.registerScope(WebApplicationContext.SCOPE_REQUEST, new RequestScope());
		beanFactory.registerScope(WebApplicationContext.SCOPE_SESSION, new SessionScope());
		if (sc != null) {
			ServletContextScope appScope = new ServletContextScope(sc);
			beanFactory.registerScope(WebApplicationContext.SCOPE_APPLICATION, appScope);
			// Register as ServletContext attribute, for ContextCleanupListener to detect it.
			sc.setAttribute(ServletContextScope.class.getName(), appScope);
		}

		beanFactory.registerResolvableDependency(ServletRequest.class, new RequestObjectFactory());
		beanFactory.registerResolvableDependency(ServletResponse.class, new ResponseObjectFactory());
		beanFactory.registerResolvableDependency(HttpSession.class, new SessionObjectFactory());
		beanFactory.registerResolvableDependency(WebRequest.class, new WebRequestObjectFactory());
		if (jsfPresent) {
			FacesDependencyRegistrar.registerFacesDependencies(beanFactory);
		}
	}
```



这个扫描包，basePackages是空的，所以这次直接跳过，第一次调用

![image-20220215182809751](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220215182809751.png)





#### 问题 说明： 

`org.springframework.beans.factory.support.DefaultListableBeanFactory#resolvableDependencies` 这个map的意义是什么？？

```java
	private final Map<Class<?>, Object> resolvableDependencies = new ConcurrentHashMap<>(16);
  比如以下操作： 
// BeanFactory interface not registered as resolvable type in a plain factory.
		// MessageSource registered (and found for autowiring) as a bean.
		beanFactory.registerResolvableDependency(BeanFactory.class, beanFactory);
		beanFactory.registerResolvableDependency(ResourceLoader.class, this);
		beanFactory.registerResolvableDependency(ApplicationEventPublisher.class, this);
		beanFactory.registerResolvableDependency(ApplicationContext.class, this);

    //org.springframework.web.context.support.WebApplicationContextUtils#registerWebApplicationScopes(org.springframework.beans.factory.config.ConfigurableListableBeanFactory, javax.servlet.ServletContext)
    beanFactory.registerResolvableDependency(ServletRequest.class, new RequestObjectFactory());
		beanFactory.registerResolvableDependency(ServletResponse.class, new ResponseObjectFactory());
		beanFactory.registerResolvableDependency(HttpSession.class, new SessionObjectFactory());
		beanFactory.registerResolvableDependency(WebRequest.class, new WebRequestObjectFactory());
	

```

[比较好的解释](https://its201.com/article/yuge1123/106053857)

`BeanFactory`接口的实现类比较多，这里手工直接注入对应的实现，如果是自己写代码，可以直接通过@Primary或者 @Qulifier等注解指定哪个是主对象，或者哪个具体的对象名被注入了，但是在spring的启动过程中，一般采用 `registerResolvableDependency`这种方式





### 包扫描



直接初始化了两个加载读取注解器

```java
public AnnotationConfigServletWebApplicationContext(DefaultListableBeanFactory beanFactory) {
		super(beanFactory);
		this.reader = new AnnotatedBeanDefinitionReader(this);
		this.scanner = new ClassPathBeanDefinitionScanner(this);
	}
```



#### ClassPathBeanDefinitionScanner

//执行扫描指定的包

```java
/**
	 * Perform a scan within the specified base packages.
	 * @param basePackages the packages to check for annotated classes
	 * @return number of beans registered
	 */
	public int scan(String... basePackages) {
		int beanCountAtScanStart = this.registry.getBeanDefinitionCount();

		doScan(basePackages);

		// Register annotation config processors, if necessary.
		if (this.includeAnnotationConfig) {
			AnnotationConfigUtils.registerAnnotationConfigProcessors(this.registry);
		}

		return (this.registry.getBeanDefinitionCount() - beanCountAtScanStart);
	}



/**
	 * Perform a scan within the specified base packages,
	 * returning the registered bean definitions.
	 * <p>This method does <i>not</i> register an annotation config processor
	 * but rather leaves this up to the caller.
	 * @param basePackages the packages to check for annotated classes
	 * @return set of beans registered if any for tooling registration purposes (never {@code null})
	 */
	protected Set<BeanDefinitionHolder> doScan(String... basePackages) {
		Assert.notEmpty(basePackages, "At least one base package must be specified");
		Set<BeanDefinitionHolder> beanDefinitions = new LinkedHashSet<>();
		for (String basePackage : basePackages) {
      //扫描输入basePackage包及其子包对应的组件
			Set<BeanDefinition> candidates = findCandidateComponents(basePackage);
			for (BeanDefinition candidate : candidates) {
				ScopeMetadata scopeMetadata = this.scopeMetadataResolver.resolveScopeMetadata(candidate);
				candidate.setScope(scopeMetadata.getScopeName());
				String beanName = this.beanNameGenerator.generateBeanName(candidate, this.registry);
				if (candidate instanceof AbstractBeanDefinition) {
					postProcessBeanDefinition((AbstractBeanDefinition) candidate, beanName);
				}
				if (candidate instanceof AnnotatedBeanDefinition) {
					AnnotationConfigUtils.processCommonDefinitionAnnotations((AnnotatedBeanDefinition) candidate);
				}
				if (checkCandidate(beanName, candidate)) {
					BeanDefinitionHolder definitionHolder = new BeanDefinitionHolder(candidate, beanName);
					definitionHolder =
							AnnotationConfigUtils.applyScopedProxyMode(scopeMetadata, definitionHolder, this.registry);
					beanDefinitions.add(definitionHolder);
					registerBeanDefinition(definitionHolder, this.registry);
				}
			}
		}
		return beanDefinitions;
	}

```



#### findCandidateComponents

`org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider#findCandidateComponents`

![image-20220216104640841](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216104640841.png)

#### scanCandidateComponents

`org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider#scanCandidateComponents`



```java
private Set<BeanDefinition> scanCandidateComponents(String basePackage) {
		Set<BeanDefinition> candidates = new LinkedHashSet<>();
		try {
      //对应路径扫描拼接  classpath*:com.xs.micro.boot/**/*.class
			String packageSearchPath = ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX +
					resolveBasePackage(basePackage) + '/' + this.resourcePattern;
			Resource[] resources = getResourcePatternResolver().getResources(packageSearchPath);
			boolean traceEnabled = logger.isTraceEnabled();
			boolean debugEnabled = logger.isDebugEnabled();
			for (Resource resource : resources) {
				if (traceEnabled) {
					logger.trace("Scanning " + resource);
				}
				if (resource.isReadable()) {
					try {
						MetadataReader metadataReader = getMetadataReaderFactory().getMetadataReader(resource);
						if (isCandidateComponent(metadataReader)) {
							ScannedGenericBeanDefinition sbd = new ScannedGenericBeanDefinition(metadataReader);
							sbd.setSource(resource);
							if (isCandidateComponent(sbd)) {
								if (debugEnabled) {
									logger.debug("Identified candidate component class: " + resource);
								}
								candidates.add(sbd);
							}
							else {
								if (debugEnabled) {
									logger.debug("Ignored because not a concrete top-level class: " + resource);
								}
							}
						}
						else {
							if (traceEnabled) {
								logger.trace("Ignored because not matching any filter: " + resource);
							}
						}
					}
					catch (Throwable ex) {
						throw new BeanDefinitionStoreException(
								"Failed to read candidate component class: " + resource, ex);
					}
				}
				else {
					if (traceEnabled) {
						logger.trace("Ignored because not readable: " + resource);
					}
				}
			}
		}
		catch (IOException ex) {
			throw new BeanDefinitionStoreException("I/O failure during classpath scanning", ex);
		}
		return candidates;
	}
```



扫描完成以后，扫描了三个class

![image-20220216105349740](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216105349740.png)



![image-20220216105045003](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216105045003.png)



#### org.springframework.core.io.support.PathMatchingResourcePatternResolver#getResources



```java
@Override
  //输入参数 locationPattern = classpath*:com/xs/micro/boot/**/*.class
	public Resource[] getResources(String locationPattern) throws IOException {
		Assert.notNull(locationPattern, "Location pattern must not be null");
		if (locationPattern.startsWith(CLASSPATH_ALL_URL_PREFIX)) {
			// a class path resource (multiple resources for same name possible)
			if (getPathMatcher().isPattern(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()))) {
				// a class path resource pattern
        //获取路径下匹配的资源
				return findPathMatchingResources(locationPattern);
			}
			else {
				// all class path resources with the given name
				return findAllClassPathResources(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()));
			}
		}
		else {
			// Generally only look for a pattern after a prefix here,
			// and on Tomcat only after the "*/" separator for its "war:" protocol.
			int prefixEnd = (locationPattern.startsWith("war:") ? locationPattern.indexOf("*/") + 1 :
					locationPattern.indexOf(':') + 1);
			if (getPathMatcher().isPattern(locationPattern.substring(prefixEnd))) {
				// a file pattern
				return findPathMatchingResources(locationPattern);
			}
			else {
				// a single resource with the given name
				return new Resource[] {getResourceLoader().getResource(locationPattern)};
			}
		}
	}
```



#### org.springframework.core.io.support.PathMatchingResourcePatternResolver#findPathMatchingResources



```java
/**
	 * Find all resources that match the given location pattern via the
	 * Ant-style PathMatcher. Supports resources in jar files and zip files
	 * and in the file system.
	 * @param locationPattern the location pattern to match
	 * @return the result as Resource array
	 * @throws IOException in case of I/O errors
	 * @see #doFindPathMatchingJarResources
	 * @see #doFindPathMatchingFileResources
	 * @see org.springframework.util.PathMatcher
	 */
// locationPattern   classpath*:com/xs/micro/boot/**/*.class
	protected Resource[] findPathMatchingResources(String locationPattern) throws IOException {
    //classpath*:com/xs/micro/boot/
		String rootDirPath = determineRootDir(locationPattern);
    /  /**/*.class
		String subPattern = locationPattern.substring(rootDirPath.length());
		Resource[] rootDirResources = getResources(rootDirPath);
		Set<Resource> result = new LinkedHashSet<>(16);
		for (Resource rootDirResource : rootDirResources) {
			rootDirResource = resolveRootDirResource(rootDirResource);
			URL rootDirUrl = rootDirResource.getURL();
			if (equinoxResolveMethod != null && rootDirUrl.getProtocol().startsWith("bundle")) {
				URL resolvedUrl = (URL) ReflectionUtils.invokeMethod(equinoxResolveMethod, null, rootDirUrl);
				if (resolvedUrl != null) {
					rootDirUrl = resolvedUrl;
				}
				rootDirResource = new UrlResource(rootDirUrl);
			}
			if (rootDirUrl.getProtocol().startsWith(ResourceUtils.URL_PROTOCOL_VFS)) {
				result.addAll(VfsResourceMatchingDelegate.findMatchingResources(rootDirUrl, subPattern, getPathMatcher()));
			}
			else if (ResourceUtils.isJarURL(rootDirUrl) || isJarResource(rootDirResource)) {
				result.addAll(doFindPathMatchingJarResources(rootDirResource, rootDirUrl, subPattern));
			}
			else {
				result.addAll(doFindPathMatchingFileResources(rootDirResource, subPattern));
			}
		}
		if (logger.isTraceEnabled()) {
			logger.trace("Resolved location pattern [" + locationPattern + "] to resources " + result);
		}
		return result.toArray(new Resource[0]);
	}
```



![image-20220216110309877](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216110309877.png)



#### findAllClassPathResources



```java
/**
	 * Find all class location resources with the given location via the ClassLoader.
	 * Delegates to {@link #doFindAllClassPathResources(String)}.
	 * @param location the absolute path within the classpath
	 * @return the result as Resource array
	 * @throws IOException in case of I/O errors
	 * @see java.lang.ClassLoader#getResources
	 * @see #convertClassLoaderURL
	 */
	protected Resource[] findAllClassPathResources(String location) throws IOException {
		String path = location;
		if (path.startsWith("/")) {
			path = path.substring(1);
		}
		Set<Resource> result = doFindAllClassPathResources(path);
		if (logger.isTraceEnabled()) {
			logger.trace("Resolved classpath location [" + location + "] to resources " + result);
		}
		return result.toArray(new Resource[0]);
	}

```



#### doFindAllClassPathResources

把字符串类型的path路径，转变为 `Resource` 对象集合，方便后期直接读取对应的内容

```java
/**
	 * Find all class location resources with the given path via the ClassLoader.
	 * Called by {@link #findAllClassPathResources(String)}.
	 * @param path the absolute path within the classpath (never a leading slash)
	 * @return a mutable Set of matching Resource instances
	 * @since 4.1.1
	 */
	protected Set<Resource> doFindAllClassPathResources(String path) throws IOException {
		Set<Resource> result = new LinkedHashSet<>(16);
		ClassLoader cl = getClassLoader();
		Enumeration<URL> resourceUrls = (cl != null ? cl.getResources(path) : ClassLoader.getSystemResources(path));
		while (resourceUrls.hasMoreElements()) {
			URL url = resourceUrls.nextElement();
			result.add(convertClassLoaderURL(url));
		}
		if ("".equals(path)) {
			// The above result is likely to be incomplete, i.e. only containing file system references.
			// We need to have pointers to each of the jar files on the classpath as well...
			addAllClassLoaderJarRoots(cl, result);
		}
		return result;
	}

```

![image-20220216111417404](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216111417404.png)





#### findPathMatchingResources

**org.springframework.core.io.support.PathMatchingResourcePatternResolver#findPathMatchingResources**

```java
/**
	 * Find all resources that match the given location pattern via the
	 * Ant-style PathMatcher. Supports resources in jar files and zip files
	 * and in the file system.
	 * @param locationPattern the location pattern to match
	 * @return the result as Resource array
	 * @throws IOException in case of I/O errors
	 * @see #doFindPathMatchingJarResources
	 * @see #doFindPathMatchingFileResources
	 * @see org.springframework.util.PathMatcher
	 */
	protected Resource[] findPathMatchingResources(String locationPattern) throws IOException {
		String rootDirPath = determineRootDir(locationPattern);
		String subPattern = locationPattern.substring(rootDirPath.length());
		Resource[] rootDirResources = getResources(rootDirPath);
		Set<Resource> result = new LinkedHashSet<>(16);
		for (Resource rootDirResource : rootDirResources) {
			rootDirResource = resolveRootDirResource(rootDirResource);
			URL rootDirUrl = rootDirResource.getURL();
			if (equinoxResolveMethod != null && rootDirUrl.getProtocol().startsWith("bundle")) {
				URL resolvedUrl = (URL) ReflectionUtils.invokeMethod(equinoxResolveMethod, null, rootDirUrl);
				if (resolvedUrl != null) {
					rootDirUrl = resolvedUrl;
				}
				rootDirResource = new UrlResource(rootDirUrl);
			}
			if (rootDirUrl.getProtocol().startsWith(ResourceUtils.URL_PROTOCOL_VFS)) {
				result.addAll(VfsResourceMatchingDelegate.findMatchingResources(rootDirUrl, subPattern, getPathMatcher()));
			}
			else if (ResourceUtils.isJarURL(rootDirUrl) || isJarResource(rootDirResource)) {
				result.addAll(doFindPathMatchingJarResources(rootDirResource, rootDirUrl, subPattern));
			}
			else {
				result.addAll(doFindPathMatchingFileResources(rootDirResource, subPattern));
			}
		}
		if (logger.isTraceEnabled()) {
			logger.trace("Resolved location pattern [" + locationPattern + "] to resources " + result);
		}
		return result.toArray(new Resource[0]);
	}
```

以上执行的结果就是获取`basePackage`包下所有的类

![image-20220216111820942](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216111820942.png)



以上方法中调用如下方法： 

#### doFindPathMatchingFileResources



```java
/**
	 * Find all resources in the file system that match the given location pattern
	 * via the Ant-style PathMatcher.
	 * @param rootDirResource the root directory as Resource
	 * @param subPattern the sub pattern to match (below the root directory)
	 * @return a mutable Set of matching Resource instances
	 * @throws IOException in case of I/O errors
	 * @see #retrieveMatchingFiles
	 * @see org.springframework.util.PathMatcher
	 */
	protected Set<Resource> doFindPathMatchingFileResources(Resource rootDirResource, String subPattern)
			throws IOException {

		File rootDir;
		try {
			rootDir = rootDirResource.getFile().getAbsoluteFile();
		}
		catch (FileNotFoundException ex) {
			if (logger.isDebugEnabled()) {
				logger.debug("Cannot search for matching files underneath " + rootDirResource +
						" in the file system: " + ex.getMessage());
			}
			return Collections.emptySet();
		}
		catch (Exception ex) {
			if (logger.isInfoEnabled()) {
				logger.info("Failed to resolve " + rootDirResource + " in the file system: " + ex);
			}
			return Collections.emptySet();
		}
		return doFindMatchingFileSystemResources(rootDir, subPattern);
	}
```

执行结果： 

![image-20220216112054399](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216112054399.png)



最后一行的执行类结果如下： 

#### doFindMatchingFileSystemResources

```java
/**
	 * Find all resources in the file system that match the given location pattern
	 * via the Ant-style PathMatcher.
	 * @param rootDir the root directory in the file system
	 * @param subPattern the sub pattern to match (below the root directory)
	 * @return a mutable Set of matching Resource instances
	 * @throws IOException in case of I/O errors
	 * @see #retrieveMatchingFiles
	 * @see org.springframework.util.PathMatcher
	 */
	protected Set<Resource> doFindMatchingFileSystemResources(File rootDir, String subPattern) throws IOException {
		if (logger.isTraceEnabled()) {
			logger.trace("Looking for matching resources in directory tree [" + rootDir.getPath() + "]");
		}
		Set<File> matchingFiles = retrieveMatchingFiles(rootDir, subPattern);
		Set<Resource> result = new LinkedHashSet<>(matchingFiles.size());
		for (File file : matchingFiles) {
			result.add(new FileSystemResource(file));
		}
		return result;
	}
```



![image-20220216112324666](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216112324666.png)



#### retrieveMatchingFiles

**org.springframework.core.io.support.PathMatchingResourcePatternResolver#retrieveMatchingFiles**



```java
/**
	 * Retrieve files that match the given path pattern,
	 * checking the given directory and its subdirectories.
	 * @param rootDir the directory to start from
	 * @param pattern the pattern to match against,
	 * relative to the root directory
	 * @return a mutable Set of matching Resource instances
	 * @throws IOException if directory contents could not be retrieved
	 */
	protected Set<File> retrieveMatchingFiles(File rootDir, String pattern) throws IOException {
		if (!rootDir.exists()) {
			// Silently skip non-existing directories.
			if (logger.isDebugEnabled()) {
				logger.debug("Skipping [" + rootDir.getAbsolutePath() + "] because it does not exist");
			}
			return Collections.emptySet();
		}
		if (!rootDir.isDirectory()) {
			// Complain louder if it exists but is no directory.
			if (logger.isInfoEnabled()) {
				logger.info("Skipping [" + rootDir.getAbsolutePath() + "] because it does not denote a directory");
			}
			return Collections.emptySet();
		}
		if (!rootDir.canRead()) {
			if (logger.isInfoEnabled()) {
				logger.info("Skipping search for matching files underneath directory [" + rootDir.getAbsolutePath() +
						"] because the application is not allowed to read the directory");
			}
			return Collections.emptySet();
		}
		String fullPattern = StringUtils.replace(rootDir.getAbsolutePath(), File.separator, "/");
		if (!pattern.startsWith("/")) {
			fullPattern += "/";
		}
		fullPattern = fullPattern + StringUtils.replace(pattern, File.separator, "/");
		Set<File> result = new LinkedHashSet<>(8);
    //核心还是这个方法： 
		doRetrieveMatchingFiles(fullPattern, rootDir, result);
		return result;
	}

```



#### 最核心的处理方法doRetrieveMatchingFiles

**org.springframework.core.io.support.PathMatchingResourcePatternResolver#doRetrieveMatchingFiles**

递归扫描传入全路径下所有的类

```java
	/**
	 * Recursively retrieve files that match the given pattern,
	 * adding them to the given result list.
	 * @param fullPattern the pattern to match against,
	 * with prepended root directory path
	 * @param dir the current directory
	 * @param result the Set of matching File instances to add to
	 * @throws IOException if directory contents could not be retrieved
	 */
	protected void doRetrieveMatchingFiles(String fullPattern, File dir, Set<File> result) throws IOException {
		if (logger.isTraceEnabled()) {
			logger.trace("Searching directory [" + dir.getAbsolutePath() +
					"] for files matching pattern [" + fullPattern + "]");
		}
		for (File content : listDirectory(dir)) {
			String currPath = StringUtils.replace(content.getAbsolutePath(), File.separator, "/");
			if (content.isDirectory() && getPathMatcher().matchStart(fullPattern, currPath + "/")) {
				if (!content.canRead()) {
					if (logger.isDebugEnabled()) {
						logger.debug("Skipping subdirectory [" + dir.getAbsolutePath() +
								"] because the application is not allowed to read the directory");
					}
				}
				else {
					doRetrieveMatchingFiles(fullPattern, content, result);
				}
			}
			if (getPathMatcher().match(fullPattern, currPath)) {
				result.add(content);
			}
		}
	}

```

![image-20220216113447813](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216113447813.png)



以上大幅篇章是对于给定的`basePackage` 获取下面的所有的class 对应的 `Resource`的处理，这个处理以后，会获取到对应的class，以上大幅篇章的扫描处理主要是一句代码引出的 

`Resource[] resources = getResourcePatternResolver().getResources(packageSearchPath);`



#### ClassPathScanningCandidateComponentProvider#scanCandidateComponents

再次回到 `org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider#scanCandidateComponents`

获取的扫描结果为： 

![image-20220216114453451](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216114453451.png)



**对类进行过滤，因为这10个类中，有不少不需要注入spring ioc 中 ，比如是普通的pojo对象**



#### isCandidateComponent

**`org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider#isCandidateComponent(org.springframework.core.type.classreading.MetadataReader)`**



```java
/**
	 * Determine whether the given class does not match any exclude filter
	 * and does match at least one include filter.
	 * @param metadataReader the ASM ClassReader for the class
	 * @return whether the class qualifies as a candidate component
	 */
	protected boolean isCandidateComponent(MetadataReader metadataReader) throws IOException {
    //是否在不扫描的列表中，如果在，不是候选component
		for (TypeFilter tf : this.excludeFilters) {
			if (tf.match(metadataReader, getMetadataReaderFactory())) {
				return false;
			}
		}
   //类中包含@Component注解，或者 @ManagedBean 注解，匹配
		for (TypeFilter tf : this.includeFilters) {
			if (tf.match(metadataReader, getMetadataReaderFactory())) {
				return isConditionMatch(metadataReader);
			}
		}
		return false;
	}
```





![image-20220216115426163](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216115426163.png)



```java
  
   private final List<TypeFilter> includeFilters = new LinkedList<>();  含有@Component 和 @ManagedBean的注解

	 private final List<TypeFilter> excludeFilters = new LinkedList<>(); //ComponentScanAnnotationParse
// AutoConfigurationExcludeFilter  以及  TypeExcludeFilter


```



满足 `isCandidateComponent`方法的的类转变为 `BeanDefinition`

```java
 	ScannedGenericBeanDefinition sbd = new ScannedGenericBeanDefinition(metadataReader);
```

```shell
Generic bean: class [com.xs.micro.boot.domain.config.RestConfig]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null; defined in file [/Users/topwqp/Documents/work/projects/xiangshang/micro-spring-boot/target/classes/com/xs/micro/boot/domain/config/RestConfig.class]


Generic bean: class [com.xs.micro.boot.domain.controller.ConsumerController]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null; defined in file [/Users/topwqp/Documents/work/projects/xiangshang/micro-spring-boot/target/classes/com/xs/micro/boot/domain/controller/ConsumerController.class]


Generic bean: class [com.xs.micro.boot.domain.controller.TestController]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null; defined in file [/Users/topwqp/Documents/work/projects/xiangshang/micro-spring-boot/target/classes/com/xs/micro/boot/domain/controller/TestController.class]
```



![image-20220216141324046](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216141324046.png)



#### 再次回到： org.springframework.context.annotation.ClassPathBeanDefinitionScanner#doScan

```java
/**
	 * Perform a scan within the specified base packages,
	 * returning the registered bean definitions.
	 * <p>This method does <i>not</i> register an annotation config processor
	 * but rather leaves this up to the caller.
	 * @param basePackages the packages to check for annotated classes
	 * @return set of beans registered if any for tooling registration purposes (never {@code null})
	 */
	protected Set<BeanDefinitionHolder> doScan(String... basePackages) {
		Assert.notEmpty(basePackages, "At least one base package must be specified");
		Set<BeanDefinitionHolder> beanDefinitions = new LinkedHashSet<>();
		for (String basePackage : basePackages) {
      //扫描对应的包下类，找到满足条件比如含有注解@Component 的class并生成对应的BeanDefinition
			Set<BeanDefinition> candidates = findCandidateComponents(basePackage);
			for (BeanDefinition candidate : candidates) {
        //获取scope 比如 singleton等
				ScopeMetadata scopeMetadata = this.scopeMetadataResolver.resolveScopeMetadata(candidate);
				candidate.setScope(scopeMetadata.getScopeName());
        //通过生成器，生成bean对应的名字   比如RestConfig 生成为  restConfig
				String beanName = this.beanNameGenerator.generateBeanName(candidate, this.registry);
				if (candidate instanceof AbstractBeanDefinition) {
          //
					postProcessBeanDefinition((AbstractBeanDefinition) candidate, beanName);
				}
				if (candidate instanceof AnnotatedBeanDefinition) {
          //扫描类中的几种注解，是否包含对应的 注解，如果包含设置到BeanDefinition对应的属性上
					AnnotationConfigUtils.processCommonDefinitionAnnotations((AnnotatedBeanDefinition) candidate);
				}
        //检查register中是已经注册了对应的beanDefinition
				if (checkCandidate(beanName, candidate)) {
					BeanDefinitionHolder definitionHolder = new BeanDefinitionHolder(candidate, beanName);
					definitionHolder =
							AnnotationConfigUtils.applyScopedProxyMode(scopeMetadata, definitionHolder, this.registry);
					beanDefinitions.add(definitionHolder);
					registerBeanDefinition(definitionHolder, this.registry);
				}
			}
		}
		return beanDefinitions;
	}
```

![image-20220216142136805](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216142136805.png)



#### org.springframework.context.annotation.ClassPathBeanDefinitionScanner#postProcessBeanDefinition

设置BeanDefinition的一些默认值，比如 `BeanDefinition`默认值 参考类  `org.springframework.beans.factory.support.BeanDefinitionDefaults`

```JAVA
/**
	 * Apply further settings to the given bean definition,
	 * beyond the contents retrieved from scanning the component class.
	 * @param beanDefinition the scanned bean definition
	 * @param beanName the generated bean name for the given bean
	 */
	protected void postProcessBeanDefinition(AbstractBeanDefinition beanDefinition, String beanName) {
		beanDefinition.applyDefaults(this.beanDefinitionDefaults);
		if (this.autowireCandidatePatterns != null) {
			beanDefinition.setAutowireCandidate(PatternMatchUtils.simpleMatch(this.autowireCandidatePatterns, beanName));
		}
	}
```



#### org.springframework.context.annotation.AnnotationConfigUtils#processCommonDefinitionAnnotations(org.springframework.beans.factory.annotation.AnnotatedBeanDefinition)



```java
static void processCommonDefinitionAnnotations(AnnotatedBeanDefinition abd, AnnotatedTypeMetadata metadata) {
		AnnotationAttributes lazy = attributesFor(metadata, Lazy.class);
		if (lazy != null) {
			abd.setLazyInit(lazy.getBoolean("value"));
		}
		else if (abd.getMetadata() != metadata) {
			lazy = attributesFor(abd.getMetadata(), Lazy.class);
			if (lazy != null) {
				abd.setLazyInit(lazy.getBoolean("value"));
			}
		}

		if (metadata.isAnnotated(Primary.class.getName())) {
			abd.setPrimary(true);
		}
		AnnotationAttributes dependsOn = attributesFor(metadata, DependsOn.class);
		if (dependsOn != null) {
			abd.setDependsOn(dependsOn.getStringArray("value"));
		}

		AnnotationAttributes role = attributesFor(metadata, Role.class);
		if (role != null) {
			abd.setRole(role.getNumber("value").intValue());
		}
		AnnotationAttributes description = attributesFor(metadata, Description.class);
		if (description != null) {
			abd.setDescription(description.getString("value"));
		}
	}
```



#### org.springframework.context.annotation.ClassPathBeanDefinitionScanner#registerBeanDefinition



**//把生成好的`BeanDefinition` 放到对应的map中 比如  `private final Map<String, BeanDefinition> beanDefinitionMap = new ConcurrentHashMap<>(256);` 中**

```java
/**
	 * Register the specified bean with the given registry.
	 * <p>Can be overridden in subclasses, e.g. to adapt the registration
	 * process or to register further bean definitions for each scanned bean.
	 * @param definitionHolder the bean definition plus bean name for the bean
	 * @param registry the BeanDefinitionRegistry to register the bean with
	 */
	protected void registerBeanDefinition(BeanDefinitionHolder definitionHolder, BeanDefinitionRegistry registry) {
		BeanDefinitionReaderUtils.registerBeanDefinition(definitionHolder, registry);
	}

```

以上是整个`postProcessBeanFactory(beanFactory);`的解析完成

-----------------------



## invokeBeanFactoryPostProcessors(beanFactory);



`BeanFactory` 创建后的 后置处理器

```java
/**
	 * Instantiate and invoke all registered BeanFactoryPostProcessor beans,
	 * respecting explicit order if given.
	 * <p>Must be called before singleton instantiation.
	 */
	protected void invokeBeanFactoryPostProcessors(ConfigurableListableBeanFactory beanFactory) {
		PostProcessorRegistrationDelegate.invokeBeanFactoryPostProcessors(beanFactory, getBeanFactoryPostProcessors());

		// Detect a LoadTimeWeaver and prepare for weaving, if found in the meantime
		// (e.g. through an @Bean method registered by ConfigurationClassPostProcessor)
		if (beanFactory.getTempClassLoader() == null && beanFactory.containsBean(LOAD_TIME_WEAVER_BEAN_NAME)) {
			beanFactory.addBeanPostProcessor(new LoadTimeWeaverAwareProcessor(beanFactory));
			beanFactory.setTempClassLoader(new ContextTypeMatchClassLoader(beanFactory.getBeanClassLoader()));
		}
	}

```

![image-20220216145901983](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216145901983.png)

### BeanFactoryPostProcessor

`BeanFactoryPostProcessor`  代表在 BeanFactory 加载完所有的BeanDefinition后，但所有的bean都被转换为： `BeanDefinition`  但所有的bean都没有进行初始化，都只是生成BeanDefinition，这时候可以通过实现`BeanFactoryPostProcessor`进行扩展，修改`BeanDefinition`的一些注册接口

```java
@FunctionalInterface
public interface BeanFactoryPostProcessor {

	/**
	 * Modify the application context's internal bean factory after its standard
	 * initialization. All bean definitions will have been loaded, but no beans
	 * will have been instantiated yet. This allows for overriding or adding
	 * properties even to eager-initializing beans.
	 * @param beanFactory the bean factory used by the application context
	 * @throws org.springframework.beans.BeansException in case of errors
	 */
	void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException;

}

```



#### BeanDefinitionRegistryPostProcessor extends  BeanFactoryPostProcessor



这个才是扩展`BeanDefinition` 如添加BeanDefinition 和 修改BeanDefinition的比较友好的扩展接口

```java
/**
 * Extension to the standard {@link BeanFactoryPostProcessor} SPI, allowing for
 * the registration of further bean definitions <i>before</i> regular
 * BeanFactoryPostProcessor detection kicks in. In particular,
 * BeanDefinitionRegistryPostProcessor may register further bean definitions
 * which in turn define BeanFactoryPostProcessor instances.
 *
 * @author Juergen Hoeller
 * @since 3.0.1
 * @see org.springframework.context.annotation.ConfigurationClassPostProcessor
 */
public interface BeanDefinitionRegistryPostProcessor extends BeanFactoryPostProcessor {

   /**
    * Modify the application context's internal bean definition registry after its
    * standard initialization. All regular bean definitions will have been loaded,
    * but no beans will have been instantiated yet. This allows for adding further
    * bean definitions before the next post-processing phase kicks in.
    * @param registry the bean definition registry used by the application context
    * @throws org.springframework.beans.BeansException in case of errors
    */
   void postProcessBeanDefinitionRegistry(BeanDefinitionRegistry registry) throws BeansException;

}
```



比如 `org.springframework.context.annotation.ConfigurationClassPostProcessor#processConfigBeanDefinitions`就是其中一个实现，对于BeanDefinition的定义修改处理等



#### 继续回到PostProcessorRegistrationDelegate#invokeBeanFactoryPostProcessors



这段代码的意义是 把实现了分离和按照实现不同优先级接口的方式去执行

![image-20220216153217460](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216153217460.png)

```java
public static void invokeBeanFactoryPostProcessors(
			ConfigurableListableBeanFactory beanFactory, List<BeanFactoryPostProcessor> beanFactoryPostProcessors) {

		// Invoke BeanDefinitionRegistryPostProcessors first, if any.
		Set<String> processedBeans = new HashSet<>();

    //beanFactory 肯定是BeanDefinitionRegistry类型
		if (beanFactory instanceof BeanDefinitionRegistry) {
			BeanDefinitionRegistry registry = (BeanDefinitionRegistry) beanFactory;
			List<BeanFactoryPostProcessor> regularPostProcessors = new ArrayList<>();
			List<BeanDefinitionRegistryPostProcessor> registryProcessors = new ArrayList<>();

      //把BeanDefinitionRegistryPostProcessor  和 BeanFactoryPostProcessor 两种类型区分开
			for (BeanFactoryPostProcessor postProcessor : beanFactoryPostProcessors) {
				if (postProcessor instanceof BeanDefinitionRegistryPostProcessor) {
					BeanDefinitionRegistryPostProcessor registryProcessor =
							(BeanDefinitionRegistryPostProcessor) postProcessor;
					registryProcessor.postProcessBeanDefinitionRegistry(registry);
					registryProcessors.add(registryProcessor);
				}
				else {
					regularPostProcessors.add(postProcessor);
				}
			}

			// Do not initialize FactoryBeans here: We need to leave all regular beans
			// uninitialized to let the bean factory post-processors apply to them!
			// Separate between BeanDefinitionRegistryPostProcessors that implement
			// PriorityOrdered, Ordered, and the rest.
			List<BeanDefinitionRegistryPostProcessor> currentRegistryProcessors = new ArrayList<>();

			// First, invoke the BeanDefinitionRegistryPostProcessors that implement PriorityOrdered.
			String[] postProcessorNames =
					beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);
			for (String ppName : postProcessorNames) {
				if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
					currentRegistryProcessors.add(beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class));
					processedBeans.add(ppName);
				}
			}
			sortPostProcessors(currentRegistryProcessors, beanFactory);
			registryProcessors.addAll(currentRegistryProcessors);
      //优先调用实现了 BeanDefinitionRegistryPostProcessor  和 PriorityOrdered 的类比如     ConfigurationClassPostProcessor
			invokeBeanDefinitionRegistryPostProcessors(currentRegistryProcessors, registry);
			currentRegistryProcessors.clear();

			// Next, invoke the BeanDefinitionRegistryPostProcessors that implement Ordered.
			postProcessorNames = beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);
			for (String ppName : postProcessorNames) {
				if (!processedBeans.contains(ppName) && beanFactory.isTypeMatch(ppName, Ordered.class)) {
					currentRegistryProcessors.add(beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class));
					processedBeans.add(ppName);
				}
			}
			sortPostProcessors(currentRegistryProcessors, beanFactory);
			registryProcessors.addAll(currentRegistryProcessors);
			invokeBeanDefinitionRegistryPostProcessors(currentRegistryProcessors, registry);
			currentRegistryProcessors.clear();

			// Finally, invoke all other BeanDefinitionRegistryPostProcessors until no further ones appear.
			boolean reiterate = true;
			while (reiterate) {
				reiterate = false;
				postProcessorNames = beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);
				for (String ppName : postProcessorNames) {
					if (!processedBeans.contains(ppName)) {
						currentRegistryProcessors.add(beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class));
						processedBeans.add(ppName);
						reiterate = true;
					}
				}
				sortPostProcessors(currentRegistryProcessors, beanFactory);
				registryProcessors.addAll(currentRegistryProcessors);
				invokeBeanDefinitionRegistryPostProcessors(currentRegistryProcessors, registry);
				currentRegistryProcessors.clear();
			}

			// Now, invoke the postProcessBeanFactory callback of all processors handled so far.
			invokeBeanFactoryPostProcessors(registryProcessors, beanFactory);
			invokeBeanFactoryPostProcessors(regularPostProcessors, beanFactory);
		}

		else {
			// Invoke factory processors registered with the context instance.
			invokeBeanFactoryPostProcessors(beanFactoryPostProcessors, beanFactory);
		}

		// Do not initialize FactoryBeans here: We need to leave all regular beans
		// uninitialized to let the bean factory post-processors apply to them!
		String[] postProcessorNames =
				beanFactory.getBeanNamesForType(BeanFactoryPostProcessor.class, true, false);

		// Separate between BeanFactoryPostProcessors that implement PriorityOrdered,
		// Ordered, and the rest.
		List<BeanFactoryPostProcessor> priorityOrderedPostProcessors = new ArrayList<>();
		List<String> orderedPostProcessorNames = new ArrayList<>();
		List<String> nonOrderedPostProcessorNames = new ArrayList<>();
		for (String ppName : postProcessorNames) {
			if (processedBeans.contains(ppName)) {
				// skip - already processed in first phase above
			}
			else if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
				priorityOrderedPostProcessors.add(beanFactory.getBean(ppName, BeanFactoryPostProcessor.class));
			}
			else if (beanFactory.isTypeMatch(ppName, Ordered.class)) {
				orderedPostProcessorNames.add(ppName);
			}
			else {
				nonOrderedPostProcessorNames.add(ppName);
			}
		}

		// First, invoke the BeanFactoryPostProcessors that implement PriorityOrdered.
		sortPostProcessors(priorityOrderedPostProcessors, beanFactory);
		invokeBeanFactoryPostProcessors(priorityOrderedPostProcessors, beanFactory);

		// Next, invoke the BeanFactoryPostProcessors that implement Ordered.
		List<BeanFactoryPostProcessor> orderedPostProcessors = new ArrayList<>(orderedPostProcessorNames.size());
		for (String postProcessorName : orderedPostProcessorNames) {
			orderedPostProcessors.add(beanFactory.getBean(postProcessorName, BeanFactoryPostProcessor.class));
		}
		sortPostProcessors(orderedPostProcessors, beanFactory);
		invokeBeanFactoryPostProcessors(orderedPostProcessors, beanFactory);

		// Finally, invoke all other BeanFactoryPostProcessors.
		List<BeanFactoryPostProcessor> nonOrderedPostProcessors = new ArrayList<>(nonOrderedPostProcessorNames.size());
		for (String postProcessorName : nonOrderedPostProcessorNames) {
			nonOrderedPostProcessors.add(beanFactory.getBean(postProcessorName, BeanFactoryPostProcessor.class));
		}
		invokeBeanFactoryPostProcessors(nonOrderedPostProcessors, beanFactory);

		// Clear cached merged bean definitions since the post-processors might have
		// modified the original metadata, e.g. replacing placeholders in values...
		beanFactory.clearMetadataCache();
	}
```



![image-20220216153526965](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216153526965.png)



注意看这里：

这个类是实现了`CachingMetadataReaderFactoryPostProcessor`  实现了 BeanDefinitionRegistryPostProcessor` 

 `private static class CachingMetadataReaderFactoryPostProcessor
      implements BeanDefinitionRegistryPostProcessor, PriorityOrdered {`



这个类的这段代码很关键： 

```java
/**
	 * {@link BeanDefinitionRegistryPostProcessor} to register the
	 * {@link CachingMetadataReaderFactory} and configure the
	 * {@link ConfigurationClassPostProcessor}.
	 */
	private static class CachingMetadataReaderFactoryPostProcessor
			implements BeanDefinitionRegistryPostProcessor, PriorityOrdered {

		@Override
		public int getOrder() {
			// Must happen before the ConfigurationClassPostProcessor is created
			return Ordered.HIGHEST_PRECEDENCE;
		}

		@Override
		public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		}

		@Override
		public void postProcessBeanDefinitionRegistry(BeanDefinitionRegistry registry) throws BeansException {
			register(registry);
			configureConfigurationClassPostProcessor(registry);
		}

		private void register(BeanDefinitionRegistry registry) {
			BeanDefinition definition = BeanDefinitionBuilder
					.genericBeanDefinition(SharedMetadataReaderFactoryBean.class, SharedMetadataReaderFactoryBean::new)
					.getBeanDefinition();
			registry.registerBeanDefinition(BEAN_NAME, definition);
		}

		private void configureConfigurationClassPostProcessor(BeanDefinitionRegistry registry) {
			try {
        //获取org.springframework.context.annotation.internalConfigurationAnnotationProcessor，这个对应的beanDefinition是Root bean: class [org.springframework.context.annotation.ConfigurationClassPostProcessor]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null
				BeanDefinition definition = registry
						.getBeanDefinition(AnnotationConfigUtils.CONFIGURATION_ANNOTATION_PROCESSOR_BEAN_NAME);
        //在beanDefintion上绑定metadataReaderFactory属性关系
				definition.getPropertyValues().add("metadataReaderFactory", new RuntimeBeanReference(BEAN_NAME));
			}
			catch (NoSuchBeanDefinitionException ex) {
			}
		}

	}
```

![image-20220216160306200](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216160306200.png)



以下是获取对应的`BeanDefinitionRegistryPostProcessor`返回结果

`beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);`

![image-20220216160938944](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216160938944.png)



通过以上获取最先调用是 `ConfigurationClassPostProcessor`



### ConfigurationClassPostProcessor

这个类的声明如下： 实现了 `BeanDefinitionRegistryPostProcessor` 和 `PriorityOrdered`接口



高优先级执行，主要用于@Configuration标注的类的解析

```java
**
 * {@link BeanFactoryPostProcessor} used for bootstrapping processing of
 * {@link Configuration @Configuration} classes.
 *
 * <p>Registered by default when using {@code <context:annotation-config/>} or
 * {@code <context:component-scan/>}. Otherwise, may be declared manually as
 * with any other BeanFactoryPostProcessor.
 *
 * <p>This post processor is priority-ordered as it is important that any
 * {@link Bean} methods declared in {@code @Configuration} classes have
 * their corresponding bean definitions registered before any other
 * {@link BeanFactoryPostProcessor} executes.
 *
 * @author Chris Beams
 * @author Juergen Hoeller
 * @author Phillip Webb
 * @since 3.0
 */
public class ConfigurationClassPostProcessor implements BeanDefinitionRegistryPostProcessor,
		PriorityOrdered, ResourceLoaderAware, BeanClassLoaderAware, EnvironmentAware {

```



#### ConfigurationClassPostProcessor#postProcessBeanFactory



```java
@Override
	public void postProcessBeanDefinitionRegistry(BeanDefinitionRegistry registry) {
		int registryId = System.identityHashCode(registry);
		if (this.registriesPostProcessed.contains(registryId)) {
			throw new IllegalStateException(
					"postProcessBeanDefinitionRegistry already called on this post-processor against " + registry);
		}
		if (this.factoriesPostProcessed.contains(registryId)) {
			throw new IllegalStateException(
					"postProcessBeanFactory already called on this post-processor against " + registry);
		}
		this.registriesPostProcessed.add(registryId);

		processConfigBeanDefinitions(registry);
	}
```



#### ConfigurationClassPostProcessor#processConfigBeanDefinitions



```java
public void processConfigBeanDefinitions(BeanDefinitionRegistry registry) {
		List<BeanDefinitionHolder> configCandidates = new ArrayList<>();
		String[] candidateNames = registry.getBeanDefinitionNames();

		for (String beanName : candidateNames) {
			BeanDefinition beanDef = registry.getBeanDefinition(beanName);
			if (beanDef.getAttribute(ConfigurationClassUtils.CONFIGURATION_CLASS_ATTRIBUTE) != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("Bean definition has already been processed as a configuration class: " + beanDef);
				}
			}
			else if (ConfigurationClassUtils.checkConfigurationClassCandidate(beanDef, this.metadataReaderFactory)) {
				configCandidates.add(new BeanDefinitionHolder(beanDef, beanName));
			}
		}

		// Return immediately if no @Configuration classes were found
		if (configCandidates.isEmpty()) {
			return;
		}

		// Sort by previously determined @Order value, if applicable
		configCandidates.sort((bd1, bd2) -> {
			int i1 = ConfigurationClassUtils.getOrder(bd1.getBeanDefinition());
			int i2 = ConfigurationClassUtils.getOrder(bd2.getBeanDefinition());
			return Integer.compare(i1, i2);
		});

		// Detect any custom bean name generation strategy supplied through the enclosing application context
		SingletonBeanRegistry sbr = null;
		if (registry instanceof SingletonBeanRegistry) {
			sbr = (SingletonBeanRegistry) registry;
			if (!this.localBeanNameGeneratorSet) {
				BeanNameGenerator generator = (BeanNameGenerator) sbr.getSingleton(
						AnnotationConfigUtils.CONFIGURATION_BEAN_NAME_GENERATOR);
				if (generator != null) {
					this.componentScanBeanNameGenerator = generator;
					this.importBeanNameGenerator = generator;
				}
			}
		}

		if (this.environment == null) {
			this.environment = new StandardEnvironment();
		}

		// Parse each @Configuration class
		ConfigurationClassParser parser = new ConfigurationClassParser(
				this.metadataReaderFactory, this.problemReporter, this.environment,
				this.resourceLoader, this.componentScanBeanNameGenerator, registry);

		Set<BeanDefinitionHolder> candidates = new LinkedHashSet<>(configCandidates);
		Set<ConfigurationClass> alreadyParsed = new HashSet<>(configCandidates.size());
		do {
			parser.parse(candidates);
			parser.validate();

			Set<ConfigurationClass> configClasses = new LinkedHashSet<>(parser.getConfigurationClasses());
			configClasses.removeAll(alreadyParsed);

			// Read the model and create bean definitions based on its content
			if (this.reader == null) {
				this.reader = new ConfigurationClassBeanDefinitionReader(
						registry, this.sourceExtractor, this.resourceLoader, this.environment,
						this.importBeanNameGenerator, parser.getImportRegistry());
			}
			this.reader.loadBeanDefinitions(configClasses);
			alreadyParsed.addAll(configClasses);

			candidates.clear();
			if (registry.getBeanDefinitionCount() > candidateNames.length) {
				String[] newCandidateNames = registry.getBeanDefinitionNames();
				Set<String> oldCandidateNames = new HashSet<>(Arrays.asList(candidateNames));
				Set<String> alreadyParsedClasses = new HashSet<>();
				for (ConfigurationClass configurationClass : alreadyParsed) {
					alreadyParsedClasses.add(configurationClass.getMetadata().getClassName());
				}
				for (String candidateName : newCandidateNames) {
					if (!oldCandidateNames.contains(candidateName)) {
						BeanDefinition bd = registry.getBeanDefinition(candidateName);
						if (ConfigurationClassUtils.checkConfigurationClassCandidate(bd, this.metadataReaderFactory) &&
								!alreadyParsedClasses.contains(bd.getBeanClassName())) {
							candidates.add(new BeanDefinitionHolder(bd, candidateName));
						}
					}
				}
				candidateNames = newCandidateNames;
			}
		}
		while (!candidates.isEmpty());

		// Register the ImportRegistry as a bean in order to support ImportAware @Configuration classes
		if (sbr != null && !sbr.containsSingleton(IMPORT_REGISTRY_BEAN_NAME)) {
			sbr.registerSingleton(IMPORT_REGISTRY_BEAN_NAME, parser.getImportRegistry());
		}

		if (this.metadataReaderFactory instanceof CachingMetadataReaderFactory) {
			// Clear cache in externally provided MetadataReaderFactory; this is a no-op
			// for a shared cache since it'll be cleared by the ApplicationContext.
			((CachingMetadataReaderFactory) this.metadataReaderFactory).clearCache();
		}
	}
```



以上源码分块解读： 

```java
		for (String beanName : candidateNames) {
			BeanDefinition beanDef = registry.getBeanDefinition(beanName);
			if (beanDef.getAttribute(ConfigurationClassUtils.CONFIGURATION_CLASS_ATTRIBUTE) != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("Bean definition has already been processed as a configuration class: " + beanDef);
				}
			}
			else if (ConfigurationClassUtils.checkConfigurationClassCandidate(beanDef, this.metadataReaderFactory)) {
				configCandidates.add(new BeanDefinitionHolder(beanDef, beanName));
			}
		}

		// Return immediately if no @Configuration classes were found
		if (configCandidates.isEmpty()) {
			return;
		}

// Sort by previously determined @Order value, if applicable
		configCandidates.sort((bd1, bd2) -> {
			int i1 = ConfigurationClassUtils.getOrder(bd1.getBeanDefinition());
			int i2 = ConfigurationClassUtils.getOrder(bd2.getBeanDefinition());
			return Integer.compare(i1, i2);
		});
```



##### ConfigurationClassUtils.checkConfigurationClassCandidate

源码比较长，就是对不同类型的注解进行分类： 

full类型：标注了`@Configuration` 

lite类型： 标注了： @Component` 、`@ComponentScan` 、`@Import` 、`@ImportResource，以及 `@Configuration` 中标注 `@Bean` 的类



```java
public static boolean checkConfigurationClassCandidate(
			BeanDefinition beanDef, MetadataReaderFactory metadataReaderFactory) {

		String className = beanDef.getBeanClassName();
		if (className == null || beanDef.getFactoryMethodName() != null) {
			return false;
		}

		AnnotationMetadata metadata;
		if (beanDef instanceof AnnotatedBeanDefinition &&
				className.equals(((AnnotatedBeanDefinition) beanDef).getMetadata().getClassName())) {
			// Can reuse the pre-parsed metadata from the given BeanDefinition...
			metadata = ((AnnotatedBeanDefinition) beanDef).getMetadata();
		}
		else if (beanDef instanceof AbstractBeanDefinition && ((AbstractBeanDefinition) beanDef).hasBeanClass()) {
			// Check already loaded Class if present...
			// since we possibly can't even load the class file for this Class.
			Class<?> beanClass = ((AbstractBeanDefinition) beanDef).getBeanClass();
			if (BeanFactoryPostProcessor.class.isAssignableFrom(beanClass) ||
					BeanPostProcessor.class.isAssignableFrom(beanClass) ||
					AopInfrastructureBean.class.isAssignableFrom(beanClass) ||
					EventListenerFactory.class.isAssignableFrom(beanClass)) {
				return false;
			}
			metadata = AnnotationMetadata.introspect(beanClass);
		}
		else {
			try {
				MetadataReader metadataReader = metadataReaderFactory.getMetadataReader(className);
				metadata = metadataReader.getAnnotationMetadata();
			}
			catch (IOException ex) {
				if (logger.isDebugEnabled()) {
					logger.debug("Could not find class file for introspecting configuration annotations: " +
							className, ex);
				}
				return false;
			}
		}

		Map<String, Object> config = metadata.getAnnotationAttributes(Configuration.class.getName());
		if (config != null && !Boolean.FALSE.equals(config.get("proxyBeanMethods"))) {
			beanDef.setAttribute(CONFIGURATION_CLASS_ATTRIBUTE, CONFIGURATION_CLASS_FULL);
		}
		else if (config != null || isConfigurationCandidate(metadata)) {
			beanDef.setAttribute(CONFIGURATION_CLASS_ATTRIBUTE, CONFIGURATION_CLASS_LITE);
		}
		else {
			return false;
		}

		// It's a full or lite configuration candidate... Let's determine the order value, if any.
		Integer order = getOrder(metadata);
		if (order != null) {
			beanDef.setAttribute(ORDER_ATTRIBUTE, order);
		}

		return true;
	}
```



**获取`BeanNameGenerateor`**

```java
// Detect any custom bean name generation strategy supplied through the enclosing application context
		SingletonBeanRegistry sbr = null;
		if (registry instanceof SingletonBeanRegistry) {
			sbr = (SingletonBeanRegistry) registry;
			if (!this.localBeanNameGeneratorSet) {
				BeanNameGenerator generator = (BeanNameGenerator) sbr.getSingleton(
						AnnotationConfigUtils.CONFIGURATION_BEAN_NAME_GENERATOR);
				if (generator != null) {
					this.componentScanBeanNameGenerator = generator;
					this.importBeanNameGenerator = generator;
				}
			}
		}
```

![image-20220216173518467](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216173518467.png)



#### 开始解析配置类和包信息

```java
// Parse each @Configuration class
		ConfigurationClassParser parser = new ConfigurationClassParser(
				this.metadataReaderFactory, this.problemReporter, this.environment,
				this.resourceLoader, this.componentScanBeanNameGenerator, registry);

		Set<BeanDefinitionHolder> candidates = new LinkedHashSet<>(configCandidates);
		Set<ConfigurationClass> alreadyParsed = new HashSet<>(configCandidates.size());
		do {
			parser.parse(candidates);
			parser.validate();

			Set<ConfigurationClass> configClasses = new LinkedHashSet<>(parser.getConfigurationClasses());
			configClasses.removeAll(alreadyParsed);

			// Read the model and create bean definitions based on its content
			if (this.reader == null) {
				this.reader = new ConfigurationClassBeanDefinitionReader(
						registry, this.sourceExtractor, this.resourceLoader, this.environment,
						this.importBeanNameGenerator, parser.getImportRegistry());
			}
			this.reader.loadBeanDefinitions(configClasses);
			alreadyParsed.addAll(configClasses);

			candidates.clear();
			if (registry.getBeanDefinitionCount() > candidateNames.length) {
				String[] newCandidateNames = registry.getBeanDefinitionNames();
				Set<String> oldCandidateNames = new HashSet<>(Arrays.asList(candidateNames));
				Set<String> alreadyParsedClasses = new HashSet<>();
				for (ConfigurationClass configurationClass : alreadyParsed) {
					alreadyParsedClasses.add(configurationClass.getMetadata().getClassName());
				}
				for (String candidateName : newCandidateNames) {
					if (!oldCandidateNames.contains(candidateName)) {
						BeanDefinition bd = registry.getBeanDefinition(candidateName);
						if (ConfigurationClassUtils.checkConfigurationClassCandidate(bd, this.metadataReaderFactory) &&
								!alreadyParsedClasses.contains(bd.getBeanClassName())) {
							candidates.add(new BeanDefinitionHolder(bd, candidateName));
						}
					}
				}
				candidateNames = newCandidateNames;
			}
		}
		while (!candidates.isEmpty());

		// Register the ImportRegistry as a bean in order to support ImportAware @Configuration classes
		if (sbr != null && !sbr.containsSingleton(IMPORT_REGISTRY_BEAN_NAME)) {
			sbr.registerSingleton(IMPORT_REGISTRY_BEAN_NAME, parser.getImportRegistry());
		}

		if (this.metadataReaderFactory instanceof CachingMetadataReaderFactory) {
			// Clear cache in externally provided MetadataReaderFactory; this is a no-op
			// for a shared cache since it'll be cleared by the ApplicationContext.
			((CachingMetadataReaderFactory) this.metadataReaderFactory).clearCache();
		}
	}
```





![image-20220216173625886](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216173625886.png)

#### ConfigurationClassParser#parse

`org.springframework.context.annotation.ConfigurationClassParser#parse(java.util.Set<org.springframework.beans.factory.config.BeanDefinitionHolder>)`



![image-20220216173844466](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216173844466.png)



#### 解析配置类： 

##### ConfigurationClassParser#processConfigurationClass

`org.springframework.context.annotation.ConfigurationClassParser#processConfigurationClass`

```java
protected void processConfigurationClass(ConfigurationClass configClass, Predicate<String> filter) throws IOException {
		if (this.conditionEvaluator.shouldSkip(configClass.getMetadata(), ConfigurationPhase.PARSE_CONFIGURATION)) {
			return;
		}

		ConfigurationClass existingClass = this.configurationClasses.get(configClass);
		if (existingClass != null) {
			if (configClass.isImported()) {
				if (existingClass.isImported()) {
					existingClass.mergeImportedBy(configClass);
				}
				// Otherwise ignore new imported config class; existing non-imported class overrides it.
				return;
			}
			else {
				// Explicit bean definition found, probably replacing an import.
				// Let's remove the old one and go with the new one.
				this.configurationClasses.remove(configClass);
				this.knownSuperclasses.values().removeIf(configClass::equals);
			}
		}

		// Recursively process the configuration class and its superclass hierarchy.
		SourceClass sourceClass = asSourceClass(configClass, filter);
		do {
			sourceClass = doProcessConfigurationClass(configClass, sourceClass, filter);
		}
		while (sourceClass != null);

		this.configurationClasses.put(configClass, configClass);
	}

```



##### ConfigurationClassParser#doProcessConfigurationClass

真正做解析的方法

`org.springframework.context.annotation.ConfigurationClassParser#doProcessConfigurationClass`

如下方法解析了不同 的注解： 比如解析： `@Component`  @PropertySources  @ComponentScan @ImportResource等

```java
protected final SourceClass doProcessConfigurationClass( 
			ConfigurationClass configClass, SourceClass sourceClass, Predicate<String> filter)
			throws IOException {

		if (configClass.getMetadata().isAnnotated(Component.class.getName())) {
			// Recursively process any member (nested) classes first
			processMemberClasses(configClass, sourceClass, filter);
		}

		// Process any @PropertySource annotations
		for (AnnotationAttributes propertySource : AnnotationConfigUtils.attributesForRepeatable(
				sourceClass.getMetadata(), PropertySources.class,
				org.springframework.context.annotation.PropertySource.class)) {
			if (this.environment instanceof ConfigurableEnvironment) {
				processPropertySource(propertySource);
			}
			else {
				logger.info("Ignoring @PropertySource annotation on [" + sourceClass.getMetadata().getClassName() +
						"]. Reason: Environment must implement ConfigurableEnvironment");
			}
		}

		// Process any @ComponentScan annotations
		Set<AnnotationAttributes> componentScans = AnnotationConfigUtils.attributesForRepeatable(
				sourceClass.getMetadata(), ComponentScans.class, ComponentScan.class);
		if (!componentScans.isEmpty() &&
				!this.conditionEvaluator.shouldSkip(sourceClass.getMetadata(), ConfigurationPhase.REGISTER_BEAN)) {
			for (AnnotationAttributes componentScan : componentScans) {
				// The config class is annotated with @ComponentScan -> perform the scan immediately
				Set<BeanDefinitionHolder> scannedBeanDefinitions =
						this.componentScanParser.parse(componentScan, sourceClass.getMetadata().getClassName());
				// Check the set of scanned definitions for any further config classes and parse recursively if needed
				for (BeanDefinitionHolder holder : scannedBeanDefinitions) {
					BeanDefinition bdCand = holder.getBeanDefinition().getOriginatingBeanDefinition();
					if (bdCand == null) {
						bdCand = holder.getBeanDefinition();
					}
					if (ConfigurationClassUtils.checkConfigurationClassCandidate(bdCand, this.metadataReaderFactory)) {
						parse(bdCand.getBeanClassName(), holder.getBeanName());
					}
				}
			}
		}

		// Process any @Import annotations
		processImports(configClass, sourceClass, getImports(sourceClass), filter, true);

		// Process any @ImportResource annotations
		AnnotationAttributes importResource =
				AnnotationConfigUtils.attributesFor(sourceClass.getMetadata(), ImportResource.class);
		if (importResource != null) {
			String[] resources = importResource.getStringArray("locations");
			Class<? extends BeanDefinitionReader> readerClass = importResource.getClass("reader");
			for (String resource : resources) {
				String resolvedResource = this.environment.resolveRequiredPlaceholders(resource);
				configClass.addImportedResource(resolvedResource, readerClass);
			}
		}

		// Process individual @Bean methods
		Set<MethodMetadata> beanMethods = retrieveBeanMethodMetadata(sourceClass);
		for (MethodMetadata methodMetadata : beanMethods) {
			configClass.addBeanMethod(new BeanMethod(methodMetadata, configClass));
		}

		// Process default methods on interfaces
		processInterfaces(configClass, sourceClass);

		// Process superclass, if any
		if (sourceClass.getMetadata().hasSuperClass()) {
			String superclass = sourceClass.getMetadata().getSuperClassName();
			if (superclass != null && !superclass.startsWith("java") &&
					!this.knownSuperclasses.containsKey(superclass)) {
				this.knownSuperclasses.put(superclass, configClass);
				// Superclass found, return its annotation metadata and recurse
				return sourceClass.getSuperClass();
			}
		}

		// No superclass -> processing is complete
		return null;
	}

```



注意这个

![image-20220216175352336](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216175352336.png)



### @ComponentScan注解解析

```java
	Set<BeanDefinitionHolder> scannedBeanDefinitions =
						this.componentScanParser.parse(componentScan, sourceClass.getMetadata().getClassName());
			
```

#### org.springframework.context.annotation.ComponentScanAnnotationParser#parse



```java
public Set<BeanDefinitionHolder> parse(AnnotationAttributes componentScan, final String declaringClass) {
	//初始化类路径解析器
  ClassPathBeanDefinitionScanner scanner = new ClassPathBeanDefinitionScanner(this.registry,
				componentScan.getBoolean("useDefaultFilters"), this.environment, this.resourceLoader);

    //命名生成器
		Class<? extends BeanNameGenerator> generatorClass = componentScan.getClass("nameGenerator");
		boolean useInheritedGenerator = (BeanNameGenerator.class == generatorClass);
		scanner.setBeanNameGenerator(useInheritedGenerator ? this.beanNameGenerator :
				BeanUtils.instantiateClass(generatorClass));

		ScopedProxyMode scopedProxyMode = componentScan.getEnum("scopedProxy");
		if (scopedProxyMode != ScopedProxyMode.DEFAULT) {
			scanner.setScopedProxyMode(scopedProxyMode);
		}
		else {
			Class<? extends ScopeMetadataResolver> resolverClass = componentScan.getClass("scopeResolver");
			scanner.setScopeMetadataResolver(BeanUtils.instantiateClass(resolverClass));
		}

		scanner.setResourcePattern(componentScan.getString("resourcePattern"));

		for (AnnotationAttributes filter : componentScan.getAnnotationArray("includeFilters")) {
			for (TypeFilter typeFilter : typeFiltersFor(filter)) {
				scanner.addIncludeFilter(typeFilter);
			}
		}
		for (AnnotationAttributes filter : componentScan.getAnnotationArray("excludeFilters")) {
			for (TypeFilter typeFilter : typeFiltersFor(filter)) {
				scanner.addExcludeFilter(typeFilter);
			}
		}

		boolean lazyInit = componentScan.getBoolean("lazyInit");
		if (lazyInit) {
			scanner.getBeanDefinitionDefaults().setLazyInit(true);
		}

		Set<String> basePackages = new LinkedHashSet<>();
		String[] basePackagesArray = componentScan.getStringArray("basePackages");
		for (String pkg : basePackagesArray) {
			String[] tokenized = StringUtils.tokenizeToStringArray(this.environment.resolvePlaceholders(pkg),
					ConfigurableApplicationContext.CONFIG_LOCATION_DELIMITERS);
			Collections.addAll(basePackages, tokenized);
		}
		for (Class<?> clazz : componentScan.getClassArray("basePackageClasses")) {
			basePackages.add(ClassUtils.getPackageName(clazz));
		}
		if (basePackages.isEmpty()) {
			basePackages.add(ClassUtils.getPackageName(declaringClass));
		}

		scanner.addExcludeFilter(new AbstractTypeHierarchyTraversingFilter(false, false) {
			@Override
			protected boolean matchClassName(String className) {
				return declaringClass.equals(className);
			}
		});
		return scanner.doScan(StringUtils.toStringArray(basePackages));
	}
```



核心在最后一句： `scanner.doScan(StringUtils.toStringArray(basePackages))`



#### org.springframework.context.annotation.ClassPathBeanDefinitionScanner#doScan

```java
protected Set<BeanDefinitionHolder> doScan(String... basePackages) {
		Assert.notEmpty(basePackages, "At least one base package must be specified");
		Set<BeanDefinitionHolder> beanDefinitions = new LinkedHashSet<>();
		for (String basePackage : basePackages) {
			Set<BeanDefinition> candidates = findCandidateComponents(basePackage);
			for (BeanDefinition candidate : candidates) {
				ScopeMetadata scopeMetadata = this.scopeMetadataResolver.resolveScopeMetadata(candidate);
				candidate.setScope(scopeMetadata.getScopeName());
				String beanName = this.beanNameGenerator.generateBeanName(candidate, this.registry);
				if (candidate instanceof AbstractBeanDefinition) {
					postProcessBeanDefinition((AbstractBeanDefinition) candidate, beanName);
				}
				if (candidate instanceof AnnotatedBeanDefinition) {
					AnnotationConfigUtils.processCommonDefinitionAnnotations((AnnotatedBeanDefinition) candidate);
				}
				if (checkCandidate(beanName, candidate)) {
					BeanDefinitionHolder definitionHolder = new BeanDefinitionHolder(candidate, beanName);
					definitionHolder =
							AnnotationConfigUtils.applyScopedProxyMode(scopeMetadata, definitionHolder, this.registry);
					beanDefinitions.add(definitionHolder);
					registerBeanDefinition(definitionHolder, this.registry);
				}
			}
		}
		return beanDefinitions;
	}
```



![image-20220216180022094](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216180022094.png)



这里的调用链很深，也会调用到`org.springframework.context.annotation.AnnotationConfigUtils#processCommonDefinitionAnnotations`方法，解析其中的注解信息



最后会调用到 `org.springframework.context.annotation.ComponentScanAnnotationParser#parse`

![image-20220216181024986](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216181024986.png)



这个方法很长，里面的每一种类型注解 @Bean  @ImportResource  @Import 等需要单独debug进行查看，后期有时间了，单独一个不同bean的注解的加载

```java
@Nullable
	protected final SourceClass doProcessConfigurationClass(
			ConfigurationClass configClass, SourceClass sourceClass, Predicate<String> filter)
			throws IOException {

		if (configClass.getMetadata().isAnnotated(Component.class.getName())) {
			// Recursively process any member (nested) classes first
			processMemberClasses(configClass, sourceClass, filter);
		}

		// Process any @PropertySource annotations
		for (AnnotationAttributes propertySource : AnnotationConfigUtils.attributesForRepeatable(
				sourceClass.getMetadata(), PropertySources.class,
				org.springframework.context.annotation.PropertySource.class)) {
			if (this.environment instanceof ConfigurableEnvironment) {
				processPropertySource(propertySource);
			}
			else {
				logger.info("Ignoring @PropertySource annotation on [" + sourceClass.getMetadata().getClassName() +
						"]. Reason: Environment must implement ConfigurableEnvironment");
			}
		}

		// Process any @ComponentScan annotations
		Set<AnnotationAttributes> componentScans = AnnotationConfigUtils.attributesForRepeatable(
				sourceClass.getMetadata(), ComponentScans.class, ComponentScan.class);
		if (!componentScans.isEmpty() &&
				!this.conditionEvaluator.shouldSkip(sourceClass.getMetadata(), ConfigurationPhase.REGISTER_BEAN)) {
			for (AnnotationAttributes componentScan : componentScans) {
				// The config class is annotated with @ComponentScan -> perform the scan immediately
				Set<BeanDefinitionHolder> scannedBeanDefinitions =
						this.componentScanParser.parse(componentScan, sourceClass.getMetadata().getClassName());
				// Check the set of scanned definitions for any further config classes and parse recursively if needed
				for (BeanDefinitionHolder holder : scannedBeanDefinitions) {
					BeanDefinition bdCand = holder.getBeanDefinition().getOriginatingBeanDefinition();
					if (bdCand == null) {
						bdCand = holder.getBeanDefinition();
					}
					if (ConfigurationClassUtils.checkConfigurationClassCandidate(bdCand, this.metadataReaderFactory)) {
						parse(bdCand.getBeanClassName(), holder.getBeanName());
					}
				}
			}
		}

		// Process any @Import annotations
		processImports(configClass, sourceClass, getImports(sourceClass), filter, true);

		// Process any @ImportResource annotations
		AnnotationAttributes importResource =
				AnnotationConfigUtils.attributesFor(sourceClass.getMetadata(), ImportResource.class);
		if (importResource != null) {
			String[] resources = importResource.getStringArray("locations");
			Class<? extends BeanDefinitionReader> readerClass = importResource.getClass("reader");
			for (String resource : resources) {
				String resolvedResource = this.environment.resolveRequiredPlaceholders(resource);
				configClass.addImportedResource(resolvedResource, readerClass);
			}
		}

		// Process individual @Bean methods
		Set<MethodMetadata> beanMethods = retrieveBeanMethodMetadata(sourceClass);
		for (MethodMetadata methodMetadata : beanMethods) {
			configClass.addBeanMethod(new BeanMethod(methodMetadata, configClass));
		}

		// Process default methods on interfaces
		processInterfaces(configClass, sourceClass);

		// Process superclass, if any
		if (sourceClass.getMetadata().hasSuperClass()) {
			String superclass = sourceClass.getMetadata().getSuperClassName();
			if (superclass != null && !superclass.startsWith("java") &&
					!this.knownSuperclasses.containsKey(superclass)) {
				this.knownSuperclasses.put(superclass, configClass);
				// Superclass found, return its annotation metadata and recurse
				return sourceClass.getSuperClass();
			}
		}

		// No superclass -> processing is complete
		return null;
	}

```



这里做个总结，关于class的加载和处理，是分多个阶段处理的，比如先把 类的路径 'com.xs.spring.boot.RestConfig' 字符串转变为class， 再在不同阶段对class内部注解进行解析，比较全面解析完不同的注解，把真正的BeanDefinition更加充实化



这一切还只是在丰富BeanDefinition的定义，还没有进行bean的初始化处理





### 加载注解@SpringBootApplication



在这时，才真正加载对应的配置

![image-20220216183323447](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216183323447.png)



在解析导入的class时，解析到`com.xs.micro.boot.MicroBootApplication`时，会解析对应的 注解： `@SpringBootApplication`，一步一步深入，会解析到@Import 开始把所有的注解都解析一遍，这时候开始加载 `@EnableAutoConfiguration`  对应的注解对应的类，开始分批加载解析

在解析`@Import注解时才真正开始注入这些类

![image-20220216183820562](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220216183820562.png)



以上解析完成所有的类和注解到 `BeanDefinition`中，为下一步初始化做准备

`AbstractApplicationContext` 类中的 refresh方法中 执行 invokeBeanFactoryPostProcessors(beanFactory); 解析完毕

到这里 `invokeBeanFactoryPostProcessors(beanFactory);`



```java
@Override
	public void refresh() throws BeansException, IllegalStateException {
		synchronized (this.startupShutdownMonitor) {
			// Prepare this context for refreshing.
			prepareRefresh();

			// Tell the subclass to refresh the internal bean factory.
			ConfigurableListableBeanFactory beanFactory = obtainFreshBeanFactory();

			// Prepare the bean factory for use in this context.
			prepareBeanFactory(beanFactory);

			try {
				// Allows post-processing of the bean factory in context subclasses.
				postProcessBeanFactory(beanFactory);

				// Invoke factory processors registered as beans in the context.
        //解析注解把class解析为BeanDefinition，为下一步初始化做准备
				invokeBeanFactoryPostProcessors(beanFactory);

				// Register bean processors that intercept bean creation.
				registerBeanPostProcessors(beanFactory);

				// Initialize message source for this context.
				initMessageSource();

				// Initialize event multicaster for this context.
				initApplicationEventMulticaster();

				// Initialize other special beans in specific context subclasses.
				onRefresh();

				// Check for listener beans and register them.
				registerListeners();

				// Instantiate all remaining (non-lazy-init) singletons.
				finishBeanFactoryInitialization(beanFactory);

				// Last step: publish corresponding event.
				finishRefresh();
			}

			catch (BeansException ex) {
				if (logger.isWarnEnabled()) {
					logger.warn("Exception encountered during context initialization - " +
							"cancelling refresh attempt: " + ex);
				}

				// Destroy already created singletons to avoid dangling resources.
				destroyBeans();

				// Reset 'active' flag.
				cancelRefresh(ex);

				// Propagate exception to caller.
				throw ex;
			}

			finally {
				// Reset common introspection caches in Spring's core, since we
				// might not ever need metadata for singleton beans anymore...
				resetCommonCaches();
			}
		}
	}
```



debug的一些点： 

![image-20220217100007151](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217100007151.png)