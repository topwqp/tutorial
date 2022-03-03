

## AbstractApplicationContext.refresh() 方法分析

```java
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
```



### 前置条件

调用到`registerBeanPostProcessors` 方法之前，我们来看一下 BeanFactory中相关的信息，相当于在这里打一个节点，说明一下，我们进入到`registerBeanPostProcessors`分析之前的前置条件是什么？？

#### 前置条件大预览

直接debug时，通过evaluate 出 `beanFactory`的信息

![image-20220217102420288](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217102420288.png)



#### beanDefinitionMap

```
 size = 128
```

![image-20220217101516746](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217101516746.png)

```java
这里放一套debug的比较全的BeanDefinition 的信息方便后期查看分析，在这之前最重要的成果就是通过BeanFactoryPostProcessor把class解析为 BeanDefinition
  
注意，这个是不全的，关于beanDefinition的信息，比较全的还是需要直接debug看，更方便，比如一个重要的信息  internalConfigurationAnnotationProcessor  这个bean的role在debug时显示2，但是在打印中没有，我猜猜打印应该是调用的toString方法，toString方法，没有对所有的信息进行打印导致的
"defaultServletHandlerMapping" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5175} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=defaultServletHandlerMapping; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"restTemplate" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@4615} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=restConfig; factoryMethodName=restTemplate; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [com/xs/micro/boot/domain/config/RestConfig.class]"
"consumerController" -> {ScannedGenericBeanDefinition@4503} "Generic bean: class [com.xs.micro.boot.domain.controller.ConsumerController]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null; defined in file [/Users/topwqp/Documents/work/projects/xiangshang/micro-spring-boot/target/classes/com/xs/micro/boot/domain/controller/ConsumerController.class]"
"applicationTaskExecutor" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5177} "Root bean: class [null]; scope=; abstract=false; lazyInit=true; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.task.TaskExecutionAutoConfiguration; factoryMethodName=applicationTaskExecutor; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/task/TaskExecutionAutoConfiguration.class]"
"characterEncodingFilter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5179} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.HttpEncodingAutoConfiguration; factoryMethodName=characterEncodingFilter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/HttpEncodingAutoConfiguration.class]"
"org.springframework.boot.autoconfigure.web.servlet.MultipartAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5181} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.MultipartAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration$DispatcherServletRegistrationConfiguration" -> {AnnotatedGenericBeanDefinition@5183} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration$DispatcherServletRegistrationConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"preserveErrorControllerTargetClassPostProcessor" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5185} "Root bean: class [org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=preserveErrorControllerTargetClassPostProcessor; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/error/ErrorMvcAutoConfiguration.class]"
"org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$JacksonObjectMapperBuilderConfiguration" -> {AnnotatedGenericBeanDefinition@5187} "Generic bean: class [org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$JacksonObjectMapperBuilderConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.context.annotation.internalConfigurationAnnotationProcessor" -> {RootBeanDefinition@4193} "Root bean: class [org.springframework.context.annotation.ConfigurationClassPostProcessor]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"propertySourcesPlaceholderConfigurer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@4772} "Root bean: class [org.springframework.boot.autoconfigure.context.PropertyPlaceholderAutoConfiguration]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=propertySourcesPlaceholderConfigurer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/context/PropertyPlaceholderAutoConfiguration.class]"
"beanNameViewResolver" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5191} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$WhitelabelErrorViewConfiguration; factoryMethodName=beanNameViewResolver; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/error/ErrorMvcAutoConfiguration$WhitelabelErrorViewConfiguration.class]"
"viewResolver" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5193} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter; factoryMethodName=viewResolver; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter.class]"
"stringHttpMessageConverter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5195} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration$StringHttpMessageConverterConfiguration; factoryMethodName=stringHttpMessageConverter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/http/HttpMessageConvertersAutoConfiguration$StringHttpMessageConverterConfiguration.class]"
"org.springframework.boot.autoconfigure.web.embedded.EmbeddedWebServerFactoryCustomizerAutoConfiguration$TomcatWebServerFactoryCustomizerConfiguration" -> {AnnotatedGenericBeanDefinition@5197} "Generic bean: class [org.springframework.boot.autoconfigure.web.embedded.EmbeddedWebServerFactoryCustomizerAutoConfiguration$TomcatWebServerFactoryCustomizerConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"tomcatServletWebServerFactoryCustomizer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5199} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryAutoConfiguration; factoryMethodName=tomcatServletWebServerFactoryCustomizer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/ServletWebServerFactoryAutoConfiguration.class]"
"org.springframework.boot.autoconfigure.admin.SpringApplicationAdminJmxAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5201} "Generic bean: class [org.springframework.boot.autoconfigure.admin.SpringApplicationAdminJmxAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"server-org.springframework.boot.autoconfigure.web.ServerProperties" -> {GenericBeanDefinition@5203} "Generic bean: class [org.springframework.boot.autoconfigure.web.ServerProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"messageConverters" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5205} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration; factoryMethodName=messageConverters; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/http/HttpMessageConvertersAutoConfiguration.class]"
"jsonComponentModule" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5207} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration; factoryMethodName=jsonComponentModule; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/jackson/JacksonAutoConfiguration.class]"
"websocketServletWebServerCustomizer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5209} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.websocket.servlet.WebSocketServletAutoConfiguration$TomcatWebSocketConfiguration; factoryMethodName=websocketServletWebServerCustomizer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/websocket/servlet/WebSocketServletAutoConfiguration$TomcatWebSocketConfiguration.class]"
"org.springframework.context.event.internalEventListenerFactory" -> {RootBeanDefinition@4311} "Root bean: class [org.springframework.context.event.DefaultEventListenerFactory]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.embedded.EmbeddedWebServerFactoryCustomizerAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5211} "Generic bean: class [org.springframework.boot.autoconfigure.web.embedded.EmbeddedWebServerFactoryCustomizerAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"mappingJackson2HttpMessageConverter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5213} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.http.JacksonHttpMessageConvertersConfiguration$MappingJackson2HttpMessageConverterConfiguration; factoryMethodName=mappingJackson2HttpMessageConverter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/http/JacksonHttpMessageConvertersConfiguration$MappingJackson2HttpMessageConverterConfiguration.class]"
"mbeanExporter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5215} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=true; factoryBeanName=org.springframework.boot.autoconfigure.jmx.JmxAutoConfiguration; factoryMethodName=mbeanExporter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/jmx/JmxAutoConfiguration.class]"
"mbeanServer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5217} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.jmx.JmxAutoConfiguration; factoryMethodName=mbeanServer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/jmx/JmxAutoConfiguration.class]"
"servletWebServerFactoryCustomizer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5219} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryAutoConfiguration; factoryMethodName=servletWebServerFactoryCustomizer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/ServletWebServerFactoryAutoConfiguration.class]"
"mvcUrlPathHelper" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5221} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcUrlPathHelper; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"org.springframework.boot.context.internalConfigurationPropertiesBinder" -> {GenericBeanDefinition@5223} "Generic bean: class [org.springframework.boot.context.properties.ConfigurationPropertiesBinder]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.context.internalConfigurationPropertiesBinderFactory; factoryMethodName=create; initMethodName=null; destroyMethodName=null"
"webServerFactoryCustomizerBeanPostProcessor" -> {RootBeanDefinition@5225} "Root bean: class [org.springframework.boot.web.server.WebServerFactoryCustomizerBeanPostProcessor]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.websocket.servlet.WebSocketServletAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5227} "Generic bean: class [org.springframework.boot.autoconfigure.websocket.servlet.WebSocketServletAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$Jackson2ObjectMapperBuilderCustomizerConfiguration" -> {AnnotatedGenericBeanDefinition@5229} "Generic bean: class [org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$Jackson2ObjectMapperBuilderCustomizerConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration$StringHttpMessageConverterConfiguration" -> {AnnotatedGenericBeanDefinition@5231} "Generic bean: class [org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration$StringHttpMessageConverterConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.availability.ApplicationAvailabilityAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5233} "Generic bean: class [org.springframework.boot.autoconfigure.availability.ApplicationAvailabilityAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"standardJacksonObjectMapperBuilderCustomizer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5235} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$Jackson2ObjectMapperBuilderCustomizerConfiguration; factoryMethodName=standardJacksonObjectMapperBuilderCustomizer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/jackson/JacksonAutoConfiguration$Jackson2ObjectMapperBuilderCustomizerConfiguration.class]"
"taskSchedulerBuilder" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5237} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.task.TaskSchedulingAutoConfiguration; factoryMethodName=taskSchedulerBuilder; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/task/TaskSchedulingAutoConfiguration.class]"
"org.springframework.boot.autoconfigure.task.TaskExecutionAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5239} "Generic bean: class [org.springframework.boot.autoconfigure.task.TaskExecutionAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.aop.AopAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5241} "Generic bean: class [org.springframework.boot.autoconfigure.aop.AopAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5243} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5245} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"conventionErrorViewResolver" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5247} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$DefaultErrorViewResolverConfiguration; factoryMethodName=conventionErrorViewResolver; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/error/ErrorMvcAutoConfiguration$DefaultErrorViewResolverConfiguration.class]"
"org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration" -> {AnnotatedGenericBeanDefinition@5249} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.context.event.internalEventListenerProcessor" -> {RootBeanDefinition@4281} "Root bean: class [org.springframework.context.event.EventListenerMethodProcessor]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"spring.mvc-org.springframework.boot.autoconfigure.web.servlet.WebMvcProperties" -> {GenericBeanDefinition@5251} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.WebMvcProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"localeCharsetMappingsCustomizer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5255} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.HttpEncodingAutoConfiguration; factoryMethodName=localeCharsetMappingsCustomizer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/HttpEncodingAutoConfiguration.class]"
"formContentFilter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5257} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration; factoryMethodName=formContentFilter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration.class]"
"multipartConfigElement" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5259} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.MultipartAutoConfiguration; factoryMethodName=multipartConfigElement; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/MultipartAutoConfiguration.class]"
"requestContextFilter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5261} "Root bean: class [org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=requestContextFilter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter.class]"
"testController" -> {ScannedGenericBeanDefinition@4558} "Generic bean: class [com.xs.micro.boot.domain.controller.TestController]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null; defined in file [/Users/topwqp/Documents/work/projects/xiangshang/micro-spring-boot/target/classes/com/xs/micro/boot/domain/controller/TestController.class]"
"defaultViewResolver" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5263} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter; factoryMethodName=defaultViewResolver; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter.class]"
"routerFunctionMapping" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5265} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=routerFunctionMapping; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"jacksonObjectMapperBuilder" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5267} "Root bean: class [null]; scope=prototype; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$JacksonObjectMapperBuilderConfiguration; factoryMethodName=jacksonObjectMapperBuilder; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/jackson/JacksonAutoConfiguration$JacksonObjectMapperBuilderConfiguration.class]"
"spring.task.scheduling-org.springframework.boot.autoconfigure.task.TaskSchedulingProperties" -> {GenericBeanDefinition@5269} "Generic bean: class [org.springframework.boot.autoconfigure.task.TaskSchedulingProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"restTemplateBuilder" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5271} "Root bean: class [null]; scope=; abstract=false; lazyInit=true; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.client.RestTemplateAutoConfiguration; factoryMethodName=restTemplateBuilder; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/client/RestTemplateAutoConfiguration.class]"
"multipartResolver" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5273} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.MultipartAutoConfiguration; factoryMethodName=multipartResolver; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/MultipartAutoConfiguration.class]"
"handlerFunctionAdapter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5275} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=handlerFunctionAdapter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"requestMappingHandlerMapping" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5277} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=true; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=requestMappingHandlerMapping; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"lifecycleProcessor" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5279} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.context.LifecycleAutoConfiguration; factoryMethodName=defaultLifecycleProcessor; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/context/LifecycleAutoConfiguration.class]"
"requestMappingHandlerAdapter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5281} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=requestMappingHandlerAdapter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"org.springframework.boot.autoconfigure.web.servlet.HttpEncodingAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5283} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.HttpEncodingAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"mvcHandlerMappingIntrospector" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5285} "Root bean: class [null]; scope=; abstract=false; lazyInit=true; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcHandlerMappingIntrospector; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"org.springframework.boot.autoconfigure.context.LifecycleAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5287} "Generic bean: class [org.springframework.boot.autoconfigure.context.LifecycleAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"springApplicationAdminRegistrar" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5289} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.admin.SpringApplicationAdminJmxAutoConfiguration; factoryMethodName=springApplicationAdminRegistrar; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/admin/SpringApplicationAdminJmxAutoConfiguration.class]"
"spring.info-org.springframework.boot.autoconfigure.info.ProjectInfoProperties" -> {GenericBeanDefinition@5291} "Generic bean: class [org.springframework.boot.autoconfigure.info.ProjectInfoProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.context.annotation.internalAutowiredAnnotationProcessor" -> {RootBeanDefinition@4221} "Root bean: class [org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"spring.resources-org.springframework.boot.autoconfigure.web.ResourceProperties" -> {GenericBeanDefinition@5293} "Generic bean: class [org.springframework.boot.autoconfigure.web.ResourceProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.context.properties.ConfigurationBeanFactoryMetadata" -> {GenericBeanDefinition@5295} "Generic bean: class [org.springframework.boot.context.properties.ConfigurationBeanFactoryMetadata]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5297} "Generic bean: class [org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"restConfig" -> {ScannedGenericBeanDefinition@4428} "Generic bean: class [com.xs.micro.boot.domain.config.RestConfig$$EnhancerBySpringCGLIB$$f54cfff0]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null; defined in file [/Users/topwqp/Documents/work/projects/xiangshang/micro-spring-boot/target/classes/com/xs/micro/boot/domain/config/RestConfig.class]"
"org.springframework.boot.autoconfigure.internalCachingMetadataReaderFactory" -> {GenericBeanDefinition@4400} "Generic bean: class [org.springframework.boot.autoconfigure.SharedMetadataReaderFactoryContextInitializer$SharedMetadataReaderFactoryBean]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$ParameterNamesModuleConfiguration" -> {AnnotatedGenericBeanDefinition@5299} "Generic bean: class [org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$ParameterNamesModuleConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"mvcContentNegotiationManager" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5301} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcContentNegotiationManager; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"objectNamingStrategy" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5303} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.jmx.JmxAutoConfiguration; factoryMethodName=objectNamingStrategy; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/jmx/JmxAutoConfiguration.class]"
"org.springframework.boot.autoconfigure.task.TaskSchedulingAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5305} "Generic bean: class [org.springframework.boot.autoconfigure.task.TaskSchedulingAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"errorAttributes" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5307} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration; factoryMethodName=errorAttributes; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/error/ErrorMvcAutoConfiguration.class]"
"httpRequestHandlerAdapter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5309} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=httpRequestHandlerAdapter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"beanNameHandlerMapping" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5311} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=beanNameHandlerMapping; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"spring.servlet.multipart-org.springframework.boot.autoconfigure.web.servlet.MultipartProperties" -> {GenericBeanDefinition@5313} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.MultipartProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.info.ProjectInfoAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5315} "Generic bean: class [org.springframework.boot.autoconfigure.info.ProjectInfoAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.context.annotation.internalCommonAnnotationProcessor" -> {RootBeanDefinition@4251} "Root bean: class [org.springframework.context.annotation.CommonAnnotationBeanPostProcessor]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryConfiguration$EmbeddedTomcat" -> {AnnotatedGenericBeanDefinition@5317} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryConfiguration$EmbeddedTomcat]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"resourceHandlerMapping" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5319} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=resourceHandlerMapping; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"simpleControllerHandlerAdapter" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5321} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=simpleControllerHandlerAdapter; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"spring.lifecycle-org.springframework.boot.autoconfigure.context.LifecycleProperties" -> {GenericBeanDefinition@5323} "Generic bean: class [org.springframework.boot.autoconfigure.context.LifecycleProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"parameterNamesModule" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5325} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$ParameterNamesModuleConfiguration; factoryMethodName=parameterNamesModule; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/jackson/JacksonAutoConfiguration$ParameterNamesModuleConfiguration.class]"
"org.springframework.boot.autoconfigure.web.client.RestTemplateAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5327} "Generic bean: class [org.springframework.boot.autoconfigure.web.client.RestTemplateAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5329} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"mvcValidator" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5331} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcValidator; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"org.springframework.boot.autoconfigure.websocket.servlet.WebSocketServletAutoConfiguration$TomcatWebSocketConfiguration" -> {AnnotatedGenericBeanDefinition@4821} "Generic bean: class [org.springframework.boot.autoconfigure.websocket.servlet.WebSocketServletAutoConfiguration$TomcatWebSocketConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"applicationAvailability" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5333} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.availability.ApplicationAvailabilityAutoConfiguration; factoryMethodName=applicationAvailability; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/availability/ApplicationAvailabilityAutoConfiguration.class]"
"org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5335} "Generic bean: class [org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"mvcResourceUrlProvider" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5337} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcResourceUrlProvider; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"org.springframework.boot.context.internalConfigurationPropertiesBinderFactory" -> {GenericBeanDefinition@5339} "Generic bean: class [org.springframework.boot.context.properties.ConfigurationPropertiesBinder$Factory]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"spring.task.execution-org.springframework.boot.autoconfigure.task.TaskExecutionProperties" -> {GenericBeanDefinition@5341} "Generic bean: class [org.springframework.boot.autoconfigure.task.TaskExecutionProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"viewControllerHandlerMapping" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5343} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=viewControllerHandlerMapping; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"dispatcherServlet" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5345} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration$DispatcherServletConfiguration; factoryMethodName=dispatcherServlet; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/DispatcherServletAutoConfiguration$DispatcherServletConfiguration.class]"
"org.springframework.boot.autoconfigure.AutoConfigurationPackages" -> {GenericBeanDefinition@4695} "Generic bean: class [org.springframework.boot.autoconfigure.AutoConfigurationPackages$BasePackages]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5347} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.context.properties.ConfigurationPropertiesBindingPostProcessor" -> {GenericBeanDefinition@5349} "Generic bean: class [org.springframework.boot.context.properties.ConfigurationPropertiesBindingPostProcessor]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.context.PropertyPlaceholderAutoConfiguration" -> {AnnotatedGenericBeanDefinition@4723} "Generic bean: class [org.springframework.boot.autoconfigure.context.PropertyPlaceholderAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter" -> {AnnotatedGenericBeanDefinition@5402} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"errorPageRegistrarBeanPostProcessor" -> {RootBeanDefinition@5404} "Root bean: class [org.springframework.boot.web.server.ErrorPageRegistrarBeanPostProcessor]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"errorPageCustomizer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5406} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration; factoryMethodName=errorPageCustomizer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/error/ErrorMvcAutoConfiguration.class]"
"mvcConversionService" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5408} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcConversionService; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$DefaultErrorViewResolverConfiguration" -> {AnnotatedGenericBeanDefinition@5410} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$DefaultErrorViewResolverConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.context.properties.BoundConfigurationProperties" -> {GenericBeanDefinition@5412} "Generic bean: class [org.springframework.boot.context.properties.BoundConfigurationProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"tomcatWebServerFactoryCustomizer" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5414} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.embedded.EmbeddedWebServerFactoryCustomizerAutoConfiguration$TomcatWebServerFactoryCustomizerConfiguration; factoryMethodName=tomcatWebServerFactoryCustomizer; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/embedded/EmbeddedWebServerFactoryCustomizerAutoConfiguration$TomcatWebServerFactoryCustomizerConfiguration.class]"
"microBootApplication" -> {AnnotatedGenericBeanDefinition@4339} "Generic bean: class [com.xs.micro.boot.MicroBootApplication$$EnhancerBySpringCGLIB$$5c5e6812]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.http.JacksonHttpMessageConvertersConfiguration" -> {AnnotatedGenericBeanDefinition@5416} "Generic bean: class [org.springframework.boot.autoconfigure.http.JacksonHttpMessageConvertersConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"mvcPathMatcher" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5418} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcPathMatcher; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"handlerExceptionResolver" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5420} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=handlerExceptionResolver; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"basicErrorController" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5422} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration; factoryMethodName=basicErrorController; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/error/ErrorMvcAutoConfiguration.class]"
"dispatcherServletRegistration" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5424} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration$DispatcherServletRegistrationConfiguration; factoryMethodName=dispatcherServletRegistration; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/DispatcherServletAutoConfiguration$DispatcherServletRegistrationConfiguration.class]"
"tomcatServletWebServerFactory" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5426} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryConfiguration$EmbeddedTomcat; factoryMethodName=tomcatServletWebServerFactory; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/ServletWebServerFactoryConfiguration$EmbeddedTomcat.class]"
"org.springframework.boot.autoconfigure.http.JacksonHttpMessageConvertersConfiguration$MappingJackson2HttpMessageConverterConfiguration" -> {AnnotatedGenericBeanDefinition@5428} "Generic bean: class [org.springframework.boot.autoconfigure.http.JacksonHttpMessageConvertersConfiguration$MappingJackson2HttpMessageConverterConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"spring.jackson-org.springframework.boot.autoconfigure.jackson.JacksonProperties" -> {GenericBeanDefinition@5430} "Generic bean: class [org.springframework.boot.autoconfigure.jackson.JacksonProperties]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"error" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5432} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$WhitelabelErrorViewConfiguration; factoryMethodName=defaultErrorView; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/error/ErrorMvcAutoConfiguration$WhitelabelErrorViewConfiguration.class]"
"org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5434} "Generic bean: class [org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$JacksonObjectMapperConfiguration" -> {AnnotatedGenericBeanDefinition@5436} "Generic bean: class [org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$JacksonObjectMapperConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$WhitelabelErrorViewConfiguration" -> {AnnotatedGenericBeanDefinition@5438} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$WhitelabelErrorViewConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration$DispatcherServletConfiguration" -> {AnnotatedGenericBeanDefinition@5440} "Generic bean: class [org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration$DispatcherServletConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"org.springframework.boot.autoconfigure.jmx.JmxAutoConfiguration" -> {AnnotatedGenericBeanDefinition@5442} "Generic bean: class [org.springframework.boot.autoconfigure.jmx.JmxAutoConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"mvcViewResolver" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5444} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcViewResolver; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"welcomePageHandlerMapping" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5446} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=welcomePageHandlerMapping; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"mvcUriComponentsContributor" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5448} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration; factoryMethodName=mvcUriComponentsContributor; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]"
"jacksonObjectMapper" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5450} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=true; factoryBeanName=org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$JacksonObjectMapperConfiguration; factoryMethodName=jacksonObjectMapper; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/jackson/JacksonAutoConfiguration$JacksonObjectMapperConfiguration.class]"
"org.springframework.boot.autoconfigure.aop.AopAutoConfiguration$ClassProxyingConfiguration" -> {AnnotatedGenericBeanDefinition@5452} "Generic bean: class [org.springframework.boot.autoconfigure.aop.AopAutoConfiguration$ClassProxyingConfiguration]; scope=singleton; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
"taskExecutorBuilder" -> {ConfigurationClassBeanDefinitionReader$ConfigurationClassBeanDefinition@5454} "Root bean: class [null]; scope=; abstract=false; lazyInit=null; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.boot.autoconfigure.task.TaskExecutionAutoConfiguration; factoryMethodName=taskExecutorBuilder; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/boot/autoconfigure/task/TaskExecutionAutoConfiguration.class]"
```





#### resolvableDependencies

![image-20220217101308778](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217101308778.png)



##### 备份一份resolveableDependencies的信息

```
{Class@3524} "interface javax.servlet.ServletRequest" -> {WebApplicationContextUtils$RequestObjectFactory@5066} "Current HttpServletRequest"
{Class@3410} "interface javax.servlet.http.HttpSession" -> {WebApplicationContextUtils$SessionObjectFactory@5067} "Current HttpSession"
{Class@2713} "interface org.springframework.beans.factory.BeanFactory" -> {DefaultListableBeanFactory@3381} "org.springframework.beans.factory.support.DefaultListableBeanFactory@19835e64: defining beans [org.springframework.context.annotation.internalConfigurationAnnotationProcessor,org.springframework.context.annotation.internalAutowiredAnnotationProcessor,org.springframework.context.annotation.internalCommonAnnotationProcessor,org.springframework.context.event.internalEventListenerProcessor,org.springframework.context.event.internalEventListenerFactory,microBootApplication,org.springframework.boot.autoconfigure.internalCachingMetadataReaderFactory,restConfig,consumerController,testController,restTemplate,org.springframework.boot.autoconfigure.AutoConfigurationPackages,org.springframework.boot.autoconfigure.context.PropertyPlaceholderAutoConfiguration,propertySourcesPlaceholderConfigurer,org.springframework.boot.autoconfigure.websocket.servlet.WebSocketServletAutoConfiguration$TomcatWebSocketConfiguration,websocketServletWebServerCustomizer,org.springframework.boot.autoconfigure.websocket.se"
{Class@3473} "interface javax.servlet.ServletResponse" -> {WebApplicationContextUtils$ResponseObjectFactory@5068} "Current HttpServletResponse"
{Class@3691} "interface org.springframework.web.context.request.WebRequest" -> {WebApplicationContextUtils$WebRequestObjectFactory@5069} "Current ServletWebRequest"
{Class@2717} "interface org.springframework.context.ApplicationEventPublisher" -> {AnnotationConfigServletWebServerApplicationContext@3395} "org.springframework.boot.web.servlet.context.AnnotationConfigServletWebServerApplicationContext@3f1d2e23, started on Thu Feb 17 10:01:03 CST 2022"
{Class@2031} "interface org.springframework.core.io.ResourceLoader" -> {AnnotationConfigServletWebServerApplicationContext@3395} "org.springframework.boot.web.servlet.context.AnnotationConfigServletWebServerApplicationContext@3f1d2e23, started on Thu Feb 17 10:01:03 CST 2022"
{Class@2718} "interface org.springframework.context.ApplicationContext" -> {AnnotationConfigServletWebServerApplicationContext@3395} "org.springframework.boot.web.servlet.context.AnnotationConfigServletWebServerApplicationContext@3f1d2e23, started on Thu Feb 17 10:01:03 CST 2022"
```



#### manualSingletonNames

![image-20220217102145150](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217102145150.png)

```java
0 = "autoConfigurationReport"
1 = "org.springframework.boot.context.ContextIdApplicationContextInitializer$ContextId"
2 = "springApplicationArguments"
3 = "springBootBanner"
4 = "springBootLoggingSystem"
5 = "springBootLoggerGroups"
6 = "environment"
7 = "systemProperties"
8 = "systemEnvironment"
9 = "org.springframework.context.annotation.ConfigurationClassPostProcessor.importRegistry"
```





####  ignoredDependencyInterfaces

![image-20220217102245076](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217102245076.png)

```java
0 = {Class@1656} "interface org.springframework.beans.factory.BeanFactoryAware"
1 = {Class@1773} "interface org.springframework.context.EmbeddedValueResolverAware"
2 = {Class@2781} "interface org.springframework.context.ResourceLoaderAware"
3 = {Class@3477} "interface org.springframework.beans.factory.BeanNameAware"
4 = {Class@3443} "interface org.springframework.web.context.ServletContextAware"
5 = {Class@1585} "interface org.springframework.context.ApplicationContextAware"
6 = {Class@1655} "interface org.springframework.beans.factory.BeanClassLoaderAware"
7 = {Class@2816} "interface org.springframework.context.EnvironmentAware"
8 = {Class@3490} "interface org.springframework.context.ApplicationEventPublisherAware"
9 = {Class@3533} "interface org.springframework.context.MessageSourceAware"
```



#### factoryMethodCandidateCache

```java
{Class@3877} "class org.springframework.boot.autoconfigure.context.PropertyPlaceholderAutoConfiguration" -> {Method[1]@5517} 
{Class@3905} "class org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$DefaultErrorViewResolverConfiguration" -> {Method[1]@5518} 
{Class@3893} "class org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryAutoConfiguration" -> {Method[3]@5519} 
{Class@3941} "class org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$JacksonObjectMapperConfiguration" -> {Method[1]@5520} 
{Class@3895} "class org.springframework.boot.context.properties.ConfigurationPropertiesBinder$Factory" -> {Method[2]@5521} 
{Class@3904} "class org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration$WhitelabelErrorViewConfiguration" -> {Method[2]@5522} 
{Class@3936} "class org.springframework.boot.autoconfigure.context.LifecycleAutoConfiguration" -> {Method[1]@5523} 
{Class@3951} "class org.springframework.boot.autoconfigure.web.client.RestTemplateAutoConfiguration" -> {Method[2]@5524} 
{Class@3892} "class org.springframework.boot.autoconfigure.web.servlet.ServletWebServerFactoryConfiguration$EmbeddedTomcat" -> {Method[1]@5525} 
{Class@3918} "class org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$WebMvcAutoConfigurationAdapter" -> {Method[25]@5526} 
{Class@3906} "class org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration" -> {Method[4]@5527} 
{Class@3938} "class org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$JacksonObjectMapperBuilderConfiguration" -> {Method[2]@5528} 
{Class@3933} "class org.springframework.boot.autoconfigure.availability.ApplicationAvailabilityAutoConfiguration" -> {Method[1]@5529} 
{Class@3954} "class org.springframework.boot.autoconfigure.web.servlet.MultipartAutoConfiguration" -> {Method[2]@5530} 
{Class@3947} "class org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration" -> {Method[1]@5531} 
{Class@3942} "class org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration" -> {Method[1]@5532} 
{Class@3913} "class org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration$EnableWebMvcConfiguration" -> {Method[61]@5533} 
{Class@3940} "class org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$ParameterNamesModuleConfiguration" -> {Method[1]@5534} 
{Class@3899} "class org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration$DispatcherServletRegistrationConfiguration" -> {Method[1]@5535} 
{Class@3937} "class org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration$Jackson2ObjectMapperBuilderCustomizerConfiguration" -> {Method[1]@5536} 
{Class@3922} "class org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration" -> {Method[3]@5537} 
{Class@3878} "class org.springframework.boot.autoconfigure.websocket.servlet.WebSocketServletAutoConfiguration$TomcatWebSocketConfiguration" -> {Method[1]@5538} 
{Class@3944} "class org.springframework.boot.autoconfigure.http.JacksonHttpMessageConvertersConfiguration$MappingJackson2HttpMessageConverterConfiguration" -> {Method[1]@5539} 
{Class@3742} "class com.xs.micro.boot.domain.config.RestConfig" -> {Method[1]@5540} 
{Class@3901} "class org.springframework.boot.autoconfigure.task.TaskExecutionAutoConfiguration" -> {Method[2]@5541} 
{Class@3932} "class org.springframework.boot.autoconfigure.admin.SpringApplicationAdminJmxAutoConfiguration" -> {Method[1]@5542} 
{Class@3949} "class org.springframework.boot.autoconfigure.task.TaskSchedulingAutoConfiguration" -> {Method[2]@5543} 
{Class@3896} "class org.springframework.boot.autoconfigure.web.servlet.DispatcherServletAutoConfiguration$DispatcherServletConfiguration" -> {Method[2]@5544} 
{Class@3952} "class org.springframework.boot.autoconfigure.web.embedded.EmbeddedWebServerFactoryCustomizerAutoConfiguration$TomcatWebServerFactoryCustomizerConfiguration" -> {Method[1]@5545} 
{Class@3943} "class org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration$StringHttpMessageConverterConfiguration" -> {Method[1]@5546} 
{Class@3953} "class org.springframework.boot.autoconfigure.web.servlet.HttpEncodingAutoConfiguration" -> {Method[2]@5548} 
{Class@3928} "class org.springframework.boot.autoconfigure.jmx.JmxAutoConfiguration" -> {Method[3]@5549} 
```



#### beanPostProcessors

![image-20220217102953392](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217102953392.png)

```java
0 = {ApplicationContextAwareProcessor@5357} 
1 = {ApplicationListenerDetector@5358} 
2 = {WebApplicationContextServletContextAwareProcessor@5359} 
3 = {ConfigurationClassPostProcessor$ImportAwareBeanPostProcessor@5360} 
```



#### scopse

![image-20220217103009063](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217103009063.png)







#### alreadyCreated

已经创建的基础的bean，是为了初始化`BeanDefinition`提供服务的一些bean

![image-20220217103124112](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217103124112.png)

```java
0 = "preserveErrorControllerTargetClassPostProcessor"
1 = "org.springframework.context.annotation.internalConfigurationAnnotationProcessor"
2 = "propertySourcesPlaceholderConfigurer"
3 = "org.springframework.context.event.internalEventListenerFactory"
4 = "org.springframework.context.event.internalEventListenerProcessor"
5 = "org.springframework.boot.autoconfigure.internalCachingMetadataReaderFactory"
```



#### 剩下的其他属性

![image-20220217104527142](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217104527142.png)



![image-20220217104617857](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217104617857.png)



### registerBeanPostProcessors(beanFactory);



```java
/**
	 * Instantiate and register all BeanPostProcessor beans,
	 * respecting explicit order if given.
	 * <p>Must be called before any instantiation of application beans.
	 */
//  实例化并 注册所有的BeanPostProcessor  在对象初始化前调用
	protected void registerBeanPostProcessors(ConfigurableListableBeanFactory beanFactory) {
		PostProcessorRegistrationDelegate.registerBeanPostProcessors(beanFactory, this);
	}
```



#### PostProcessorRegistrationDelegate#registerBeanPostProcessors

```java
public static void registerBeanPostProcessors(
			ConfigurableListableBeanFactory beanFactory, AbstractApplicationContext applicationContext) {

		String[] postProcessorNames = beanFactory.getBeanNamesForType(BeanPostProcessor.class, true, false);

		// Register BeanPostProcessorChecker that logs an info message when
		// a bean is created during BeanPostProcessor instantiation, i.e. when
		// a bean is not eligible for getting processed by all BeanPostProcessors.
    //说明： 这个如上图，之前有4个beanPOSTProcessor 这次又有5个BeanPostProcessor，最后为什么+1，因为下面代码还手工加了一个BeanPostProcessorChecker
		int beanProcessorTargetCount = beanFactory.getBeanPostProcessorCount() + 1 + postProcessorNames.length;
    //这个就是+1的原因
		beanFactory.addBeanPostProcessor(new BeanPostProcessorChecker(beanFactory, beanProcessorTargetCount));

		// Separate between BeanPostProcessors that implement PriorityOrdered,
		// Ordered, and the rest.
  
  //把不同的BeanPostProcessor按照 实现了 PriorityOrdered ，Ordered ，和 其他的进行分离
  // PriorityOrdered ------> priorityOrderedPostProcessors
  // MergedBeanDefinitionPostProcessor ----> internalPostProcessors
  // Ordered   ---->orderedPostProcessorNames
  // 其他  nonOrderedPostProcessorNames
		List<BeanPostProcessor> priorityOrderedPostProcessors = new ArrayList<>();
		List<BeanPostProcessor> internalPostProcessors = new ArrayList<>();
		List<String> orderedPostProcessorNames = new ArrayList<>();
		List<String> nonOrderedPostProcessorNames = new ArrayList<>();
		for (String ppName : postProcessorNames) {
			if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
				BeanPostProcessor pp = beanFactory.getBean(ppName, BeanPostProcessor.class);
				priorityOrderedPostProcessors.add(pp);
				if (pp instanceof MergedBeanDefinitionPostProcessor) {
					internalPostProcessors.add(pp);
				}
			}
			else if (beanFactory.isTypeMatch(ppName, Ordered.class)) {
				orderedPostProcessorNames.add(ppName);
			}
			else {
				nonOrderedPostProcessorNames.add(ppName);
			}
		}

		// First, register the BeanPostProcessors that implement PriorityOrdered.
   //优先注册 priorityOrderedPostProcessors
		sortPostProcessors(priorityOrderedPostProcessors, beanFactory);
		registerBeanPostProcessors(beanFactory, priorityOrderedPostProcessors);

		// Next, register the BeanPostProcessors that implement Ordered.
		List<BeanPostProcessor> orderedPostProcessors = new ArrayList<>(orderedPostProcessorNames.size());
		for (String ppName : orderedPostProcessorNames) {
			BeanPostProcessor pp = beanFactory.getBean(ppName, BeanPostProcessor.class);
			orderedPostProcessors.add(pp);
			if (pp instanceof MergedBeanDefinitionPostProcessor) {
				internalPostProcessors.add(pp);
			}
		}
   // 再注册  orderedPostProcessors
		sortPostProcessors(orderedPostProcessors, beanFactory);
		registerBeanPostProcessors(beanFactory, orderedPostProcessors);

		// Now, register all regular BeanPostProcessors.
		List<BeanPostProcessor> nonOrderedPostProcessors = new ArrayList<>(nonOrderedPostProcessorNames.size());
		for (String ppName : nonOrderedPostProcessorNames) {
			BeanPostProcessor pp = beanFactory.getBean(ppName, BeanPostProcessor.class);
			nonOrderedPostProcessors.add(pp);
			if (pp instanceof MergedBeanDefinitionPostProcessor) {
				internalPostProcessors.add(pp);
			}
		}
  // 最后注册 nonOrderedPostProcessors
		registerBeanPostProcessors(beanFactory, nonOrderedPostProcessors);

		// Finally, re-register all internal BeanPostProcessors.
		sortPostProcessors(internalPostProcessors, beanFactory);
  
     // 最后注册 实现了  MergedBeanDefinitionPostProcessor  的BeanPostProcessor
		registerBeanPostProcessors(beanFactory, internalPostProcessors);

		// Re-register post-processor for detecting inner beans as ApplicationListeners,
		// moving it to the end of the processor chain (for picking up proxies etc).
    //最后再注册ApplicationListenerDetector ,相当于移动到了BeanPostProcessor的链的末尾
		beanFactory.addBeanPostProcessor(new ApplicationListenerDetector(applicationContext));
	}

// 自定义排序方法
	private static void sortPostProcessors(List<?> postProcessors, ConfigurableListableBeanFactory beanFactory) {
		// Nothing to sort?
		if (postProcessors.size() <= 1) {
			return;
		}
		Comparator<Object> comparatorToUse = null;
		if (beanFactory instanceof DefaultListableBeanFactory) {
			comparatorToUse = ((DefaultListableBeanFactory) beanFactory).getDependencyComparator();
		}
		if (comparatorToUse == null) {
			comparatorToUse = OrderComparator.INSTANCE;
		}
		postProcessors.sort(comparatorToUse);
	}
```



以上代码和BeanFactoryPOSTProcessor的处理节奏类似，都是按照不同的优先级去分组不同的BeanPostProcessor

![image-20220217111134919](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217111134919.png)



执行第一个priorityOrderedPOSTProcessor

![image-20220217114631108](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217114631108.png)

执行priorityOrderedPOSTProcessor 后; 

![image-20220217114809569](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217114809569.png)

执行： orderedPostProcessors后，没变，因为含有orderedPostProcessors的是0个BeanPostProcessor



![image-20220217114911867](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217114911867.png)

执行：  nonOrderedPostProcessors 后

![image-20220217115036777](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217115036777.png)



执行

![image-20220217115210802](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217115210802.png)

#### 执行完以后的BeanPostProcessor

```java
0 = {ApplicationContextAwareProcessor@5169} 
1 = {WebApplicationContextServletContextAwareProcessor@5170} 
2 = {ConfigurationClassPostProcessor$ImportAwareBeanPostProcessor@5171} 
3 = {PostProcessorRegistrationDelegate$BeanPostProcessorChecker@4399} 
4 = {ConfigurationPropertiesBindingPostProcessor@4548} 
5 = {WebServerFactoryCustomizerBeanPostProcessor@4603} 
6 = {ErrorPageRegistrarBeanPostProcessor@4633} 
7 = {CommonAnnotationBeanPostProcessor@4529} 
8 = {AutowiredAnnotationBeanPostProcessor@4492} 
9 = {ApplicationListenerDetector@5172} 
```



最终执行完以后的结果是： 



![image-20220217115739756](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217115739756.png)



#### MergedBeanDefinitionPostProcessor

接口定义，没太多意义，感兴趣的自己看下

把实现类也看一下： 

![image-20220217140333937](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217140333937.png)



#### CommonAnnotationBeanPostProcessor

目前没执行，只是说明一下类的定义，方便后期看，真正创建对象的时候，才会执行这里只是简单分析

```java
	@Override
	public void postProcessMergedBeanDefinition(RootBeanDefinition beanDefinition, Class<?> beanType, String beanName) {
		super.postProcessMergedBeanDefinition(beanDefinition, beanType, beanName);
		InjectionMetadata metadata = findResourceMetadata(beanName, beanType, null);
		metadata.checkConfigMembers(beanDefinition);
	}

```



`org.springframework.context.annotation.CommonAnnotationBeanPostProcessor#buildResourceMetadata`

主要是解析bean中的一些`@Resource  @Inject 等注解`

#### ApplicationListenerDetector

目前没执行，只是说明一下类的定义，方便后期看，真正创建对象的时候才会执行，这里只是简单分析，探测一些类是ApplicationListener 类型，并且属于单例的bean 添加到对应的列表中即可，

```java
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

	@Override
	public void postProcessBeforeDestruction(Object bean, String beanName) {
		if (bean instanceof ApplicationListener) {
			try {
				ApplicationEventMulticaster multicaster = this.applicationContext.getApplicationEventMulticaster();
				multicaster.removeApplicationListener((ApplicationListener<?>) bean);
				multicaster.removeApplicationListenerBean(beanName);
			}
			catch (IllegalStateException ex) {
				// ApplicationEventMulticaster not initialized yet - no need to remove a listener
			}
		}
	}
```





### initMessageSource()

用于检测是否包含国际化相关配置，非核心不做进一步介绍

主要用于初始化国际化相关组件

```java
/**
	 * Initialize the MessageSource.
	 * Use parent's if none defined in this context.
	 */
	protected void initMessageSource() {
		ConfigurableListableBeanFactory beanFactory = getBeanFactory();
		if (beanFactory.containsLocalBean(MESSAGE_SOURCE_BEAN_NAME)) {
			this.messageSource = beanFactory.getBean(MESSAGE_SOURCE_BEAN_NAME, MessageSource.class);
			// Make MessageSource aware of parent MessageSource.
			if (this.parent != null && this.messageSource instanceof HierarchicalMessageSource) {
				HierarchicalMessageSource hms = (HierarchicalMessageSource) this.messageSource;
				if (hms.getParentMessageSource() == null) {
					// Only set parent context as parent MessageSource if no parent MessageSource
					// registered already.
					hms.setParentMessageSource(getInternalParentMessageSource());
				}
			}
			if (logger.isTraceEnabled()) {
				logger.trace("Using MessageSource [" + this.messageSource + "]");
			}
		}
		else {
			// Use empty MessageSource to be able to accept getMessage calls.
			DelegatingMessageSource dms = new DelegatingMessageSource();
			dms.setParentMessageSource(getInternalParentMessageSource());
			this.messageSource = dms;
			beanFactory.registerSingleton(MESSAGE_SOURCE_BEAN_NAME, this.messageSource);
			if (logger.isTraceEnabled()) {
				logger.trace("No '" + MESSAGE_SOURCE_BEAN_NAME + "' bean, using [" + this.messageSource + "]");
			}
		}
	}
```



### initApplicationEventMulticaster()



主要用于初始化`applicationEventMulticaster` 对象

​			`this.applicationEventMulticaster = new SimpleApplicationEventMulticaster(beanFactory);`

```java
/**
	 * Initialize the ApplicationEventMulticaster.
	 * Uses SimpleApplicationEventMulticaster if none defined in the context.
	 * @see org.springframework.context.event.SimpleApplicationEventMulticaster
	 */
	protected void initApplicationEventMulticaster() {
		ConfigurableListableBeanFactory beanFactory = getBeanFactory();
		if (beanFactory.containsLocalBean(APPLICATION_EVENT_MULTICASTER_BEAN_NAME)) {
			this.applicationEventMulticaster =
					beanFactory.getBean(APPLICATION_EVENT_MULTICASTER_BEAN_NAME, ApplicationEventMulticaster.class);
			if (logger.isTraceEnabled()) {
				logger.trace("Using ApplicationEventMulticaster [" + this.applicationEventMulticaster + "]");
			}
		}
		else {
			this.applicationEventMulticaster = new SimpleApplicationEventMulticaster(beanFactory);
			beanFactory.registerSingleton(APPLICATION_EVENT_MULTICASTER_BEAN_NAME, this.applicationEventMulticaster);
			if (logger.isTraceEnabled()) {
				logger.trace("No '" + APPLICATION_EVENT_MULTICASTER_BEAN_NAME + "' bean, using " +
						"[" + this.applicationEventMulticaster.getClass().getSimpleName() + "]");
			}
		}
	}

```



#### SimpleApplicationEventMulticaster

主要用于发送监听事件，包括同步发送和异步发送

```java
@Override
	public void multicastEvent(final ApplicationEvent event, @Nullable ResolvableType eventType) {
		ResolvableType type = (eventType != null ? eventType : resolveDefaultEventType(event));
		Executor executor = getTaskExecutor();
		for (ApplicationListener<?> listener : getApplicationListeners(event, type)) {
			if (executor != null) {
				executor.execute(() -> invokeListener(listener, event));
			}
			else {
				invokeListener(listener, event);
			}
		}
	}
```





### onRefresh()

初始化一些特定上下文的一些特定的bean，比如web上下文相关的bean

注意： 在这里，在`onRefresh` 中，初始化了tomcat相关组件，这一块关于内嵌的tomcat如何初始化，初始化了什么东西，后期单独拿出来研究，目前先大概分析一下



```java
// Initialize other special beans in specific context subclasses.
onRefresh();


/**
	 * Template method which can be overridden to add context-specific refresh work.
	 * Called on initialization of special beans, before instantiation of singletons.
	 * <p>This implementation is empty.
	 * @throws BeansException in case of errors
	 * @see #refresh()
	 */
	protected void onRefresh() throws BeansException {
		// For subclasses: do nothing by default.
	}
```

#### ServletWebServerApplicationContext#onRefresh

在`org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext#onRefresh`中有对应的实现，

```java
	@Override
	protected void onRefresh() {
		super.onRefresh();
		try {
			createWebServer();
		}
		catch (Throwable ex) {
			throw new ApplicationContextException("Unable to start web server", ex);
		}
	}
```

#### GenericWebApplicationContext#onRefresh

初始化themeSource 

`org.springframework.web.context.support.GenericWebApplicationContext#onRefresh`

#### createWebServer();



创建一些webServer相关的对象，比如 serveletContext 以及启动相关事件

```java
private void createWebServer() {
		WebServer webServer = this.webServer;
		ServletContext servletContext = getServletContext();
		if (webServer == null && servletContext == null) {
			ServletWebServerFactory factory = getWebServerFactory();
			this.webServer = factory.getWebServer(getSelfInitializer());
			getBeanFactory().registerSingleton("webServerGracefulShutdown",
					new WebServerGracefulShutdownLifecycle(this.webServer));
			getBeanFactory().registerSingleton("webServerStartStop",
					new WebServerStartStopLifecycle(this, this.webServer));
		}
		else if (servletContext != null) {
			try {
				getSelfInitializer().onStartup(servletContext);
			}
			catch (ServletException ex) {
				throw new ApplicationContextException("Cannot initialize servlet context", ex);
			}
		}
		initPropertySources();
	}

```



![image-20220217144120819](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217144120819.png)

### 内嵌的tomcat初始化



#### TomcatServletWebServerFactory#getWebServer

`org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory#getWebServer`

```java
@Override
	public WebServer getWebServer(ServletContextInitializer... initializers) {
		if (this.disableMBeanRegistry) {
			Registry.disableRegistry();
		}
		Tomcat tomcat = new Tomcat();
		File baseDir = (this.baseDirectory != null) ? this.baseDirectory : createTempDir("tomcat");
		tomcat.setBaseDir(baseDir.getAbsolutePath());
		Connector connector = new Connector(this.protocol);
		connector.setThrowOnFailure(true);
		tomcat.getService().addConnector(connector);
		customizeConnector(connector);
		tomcat.setConnector(connector);
		tomcat.getHost().setAutoDeploy(false);
		configureEngine(tomcat.getEngine());
		for (Connector additionalConnector : this.additionalTomcatConnectors) {
			tomcat.getService().addConnector(additionalConnector);
		}
		prepareContext(tomcat.getHost(), initializers);
		return getTomcatWebServer(tomcat);
	}

```

![image-20220217143741800](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217143741800.png)



#### TomcatServletWebServerFactory#getTomcatWebServer

org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory#getTomcatWebServer



```java
/**
	 * Factory method called to create the {@link TomcatWebServer}. Subclasses can
	 * override this method to return a different {@link TomcatWebServer} or apply
	 * additional processing to the Tomcat server.
	 * @param tomcat the Tomcat server.
	 * @return a new {@link TomcatWebServer} instance
	 */
	protected TomcatWebServer getTomcatWebServer(Tomcat tomcat) {
		return new TomcatWebServer(tomcat, getPort() >= 0, getShutdown());
	}
```

#### TomcatServletWebServerFactory#prepareContext

`org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory#prepareContext`

```java
protected void prepareContext(Host host, ServletContextInitializer[] initializers) {
		File documentRoot = getValidDocumentRoot();
		TomcatEmbeddedContext context = new TomcatEmbeddedContext();
		if (documentRoot != null) {
			context.setResources(new LoaderHidingResourceRoot(context));
		}
		context.setName(getContextPath());
		context.setDisplayName(getDisplayName());
		context.setPath(getContextPath());
		File docBase = (documentRoot != null) ? documentRoot : createTempDir("tomcat-docbase");
		context.setDocBase(docBase.getAbsolutePath());
		context.addLifecycleListener(new FixContextListener());
		context.setParentClassLoader((this.resourceLoader != null) ? this.resourceLoader.getClassLoader()
				: ClassUtils.getDefaultClassLoader());
		resetDefaultLocaleMapping(context);
		addLocaleMappings(context);
		try {
			context.setCreateUploadTargets(true);
		}
		catch (NoSuchMethodError ex) {
			// Tomcat is < 8.5.39. Continue.
		}
		configureTldSkipPatterns(context);
		WebappLoader loader = new WebappLoader();
		loader.setLoaderClass(TomcatEmbeddedWebappClassLoader.class.getName());
		loader.setDelegate(true);
		context.setLoader(loader);
		if (isRegisterDefaultServlet()) {
			addDefaultServlet(context);
		}
		if (shouldRegisterJspServlet()) {
			addJspServlet(context);
			addJasperInitializer(context);
		}
		context.addLifecycleListener(new StaticResourceConfigurer(context));
		ServletContextInitializer[] initializersToUse = mergeInitializers(initializers);
		host.addChild(context);
		configureContext(context, initializersToUse);
		postProcessContext(context);
	}

```



#### TomcatServletWebServerFactory#configureContext



org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory#configureContext



初始化tomcat的 context

```java
	/**
	 * Configure the Tomcat {@link Context}.
	 * @param context the Tomcat context
	 * @param initializers initializers to apply
	 */
	protected void configureContext(Context context, ServletContextInitializer[] initializers) {
		TomcatStarter starter = new TomcatStarter(initializers);
		if (context instanceof TomcatEmbeddedContext) {
			TomcatEmbeddedContext embeddedContext = (TomcatEmbeddedContext) context;
			embeddedContext.setStarter(starter);
			embeddedContext.setFailCtxIfServletStartFails(true);
		}
		context.addServletContainerInitializer(starter, NO_CLASSES);
		for (LifecycleListener lifecycleListener : this.contextLifecycleListeners) {
			context.addLifecycleListener(lifecycleListener);
		}
		for (Valve valve : this.contextValves) {
			context.getPipeline().addValve(valve);
		}
		for (ErrorPage errorPage : getErrorPages()) {
			org.apache.tomcat.util.descriptor.web.ErrorPage tomcatErrorPage = new org.apache.tomcat.util.descriptor.web.ErrorPage();
			tomcatErrorPage.setLocation(errorPage.getPath());
			tomcatErrorPage.setErrorCode(errorPage.getStatusCode());
			tomcatErrorPage.setExceptionType(errorPage.getExceptionName());
			context.addErrorPage(tomcatErrorPage);
		}
		for (MimeMappings.Mapping mapping : getMimeMappings()) {
			context.addMimeMapping(mapping.getExtension(), mapping.getMimeType());
		}
		configureSession(context);
		new DisableReferenceClearingContextCustomizer().customize(context);
		for (TomcatContextCustomizer customizer : this.tomcatContextCustomizers) {
			customizer.customize(context);
		}
	}

```



![image-20220217144531653](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217144531653.png)



![image-20220217144757800](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217144757800.png)



tomcat初始化后的信息

![image-20220217145056061](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217145056061.png)



#### registerListeners

```java
/**
	 * Add beans that implement ApplicationListener as listeners.
	 * Doesn't affect other listeners, which can be added without being beans.
	 */
	protected void registerListeners() {
		// Register statically specified listeners first.
		for (ApplicationListener<?> listener : getApplicationListeners()) {
			getApplicationEventMulticaster().addApplicationListener(listener);
		}

		// Do not initialize FactoryBeans here: We need to leave all regular beans
		// uninitialized to let post-processors apply to them!
		String[] listenerBeanNames = getBeanNamesForType(ApplicationListener.class, true, false);
		for (String listenerBeanName : listenerBeanNames) {
			getApplicationEventMulticaster().addApplicationListenerBean(listenerBeanName);
		}

		// Publish early application events now that we finally have a multicaster...
		Set<ApplicationEvent> earlyEventsToProcess = this.earlyApplicationEvents;
		this.earlyApplicationEvents = null;
		if (!CollectionUtils.isEmpty(earlyEventsToProcess)) {
			for (ApplicationEvent earlyEvent : earlyEventsToProcess) {
				getApplicationEventMulticaster().multicastEvent(earlyEvent);
			}
		}
	}
```

添加一些listener

![image-20220217145301598](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217145301598.png)



## finishBeanFactoryInitialization

开启真正的bean初始化操作： 

```java
 //实例化非延迟加载的单例bean

// Instantiate all remaining (non-lazy-init) singletons.
				finishBeanFactoryInitialization(beanFactory);

```





```java
/**
	 * Finish the initialization of this context's bean factory,
	 * initializing all remaining singleton beans.
	 */
	protected void finishBeanFactoryInitialization(ConfigurableListableBeanFactory beanFactory) {
		// Initialize conversion service for this context.
    //properties文件中的字符串转换为对应的ben
		if (beanFactory.containsBean(CONVERSION_SERVICE_BEAN_NAME) &&
				beanFactory.isTypeMatch(CONVERSION_SERVICE_BEAN_NAME, ConversionService.class)) {
			beanFactory.setConversionService(
					beanFactory.getBean(CONVERSION_SERVICE_BEAN_NAME, ConversionService.class));
		}

		// Register a default embedded value resolver if no bean post-processor
		// (such as a PropertyPlaceholderConfigurer bean) registered any before:
		// at this point, primarily for resolution in annotation attribute values.
    //加载解析占位符组件
		if (!beanFactory.hasEmbeddedValueResolver()) {
			beanFactory.addEmbeddedValueResolver(strVal -> getEnvironment().resolvePlaceholders(strVal));
		}

		// Initialize LoadTimeWeaverAware beans early to allow for registering their transformers early.
    //AspectJ 相关，暂不考虑
		String[] weaverAwareNames = beanFactory.getBeanNamesForType(LoadTimeWeaverAware.class, false, false);
		for (String weaverAwareName : weaverAwareNames) {
			getBean(weaverAwareName);
		}

		// Stop using the temporary ClassLoader for type matching.
    //停止TempClassLoader的使用
		beanFactory.setTempClassLoader(null);

		// Allow for caching all bean definition metadata, not expecting further changes.
		beanFactory.freezeConfiguration();

		// Instantiate all remaining (non-lazy-init) singletons.
    //真正的初始化bean操作
		beanFactory.preInstantiateSingletons();
	}
```



### beanFactory.preInstantiateSingletons();

`org.springframework.beans.factory.support.DefaultListableBeanFactory#preInstantiateSingletons`



```java
@Override
	public void preInstantiateSingletons() throws BeansException {
		if (logger.isTraceEnabled()) {
			logger.trace("Pre-instantiating singletons in " + this);
		}

		// Iterate over a copy to allow for init methods which in turn register new bean definitions.
		// While this may not be part of the regular factory bootstrap, it does otherwise work fine.
		List<String> beanNames = new ArrayList<>(this.beanDefinitionNames);

    //开始遍历beanDefinition为下面创建bean做准备
		// Trigger initialization of all non-lazy singleton beans...
		for (String beanName : beanNames) {
      //合并父BeanFactory中同名的BeanDefinition
			RootBeanDefinition bd = getMergedLocalBeanDefinition(beanName);
      //非抽象的，是单例的，并且非延迟加载的才进行创建
			if (!bd.isAbstract() && bd.isSingleton() && !bd.isLazyInit()) {
        //如果是实现了FactoryBean，是一套单独创建流程，这里是区分开来
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
          //非实现了FactoryBean的bean，直接调用getBean进行初始化处理
					getBean(beanName);
				}
			}
		}

    //这里是代表所有单例bean创建完成以后，实现了SmartInitializingSingleton接口的bean开始处理，这是一个单独的扩展点，扩展点的意义就是等待所有单例bean创建完成，再进行处一些其他处理的扩展点
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



### getBean(beanName)

```java
@Override
	public Object getBean(String name) throws BeansException {
		return doGetBean(name, null, null, false);
	}

```



### doGetBean

熟悉spring源码的都知道，真正的做业务的操作，都是在doXXXX形式的

`org.springframework.beans.factory.support.AbstractBeanFactory#doGetBean`

```java
/**
	 * Return an instance, which may be shared or independent, of the specified bean.
	 * @param name the name of the bean to retrieve
	 * @param requiredType the required type of the bean to retrieve
	 * @param args arguments to use when creating a bean instance using explicit arguments
	 * (only applied when creating a new instance as opposed to retrieving an existing one)
	 * @param typeCheckOnly whether the instance is obtained for a type check,
	 * not for actual use
	 * @return an instance of the bean
	 * @throws BeansException if the bean could not be created
	 */
	@SuppressWarnings("unchecked")
	protected <T> T doGetBean(
			String name, @Nullable Class<T> requiredType, @Nullable Object[] args, boolean typeCheckOnly)
			throws BeansException {
    //别名到 beanName的映射，找到真正需要创建的bean
		String beanName = transformedBeanName(name);
		Object bean;

		// Eagerly check singleton cache for manually registered singletons.
    //这里先查看是否已经初始化，如果初始化了直接返回，否则继续初始化操作
		Object sharedInstance = getSingleton(beanName);
		if (sharedInstance != null && args == null) {
			if (logger.isTraceEnabled()) {
				if (isSingletonCurrentlyInCreation(beanName)) {
					logger.trace("Returning eagerly cached instance of singleton bean '" + beanName +
							"' that is not fully initialized yet - a consequence of a circular reference");
				}
				else {
					logger.trace("Returning cached instance of singleton bean '" + beanName + "'");
				}
			}
			bean = getObjectForBeanInstance(sharedInstance, name, beanName, null);
		}

		else {
			// Fail if we're already creating this bean instance:
			// We're assumably within a circular reference.
      //bean是否正在创建，这是为了解决循环引用问题，如果正在创建，抛异常
			if (isPrototypeCurrentlyInCreation(beanName)) {
				throw new BeanCurrentlyInCreationException(beanName);
			}

			// Check if bean definition exists in this factory.
    //查看父容器是否有对应的 bean（Web 服务父子容器 springmvc 容器 和 spring容器）
			BeanFactory parentBeanFactory = getParentBeanFactory();
			if (parentBeanFactory != null && !containsBeanDefinition(beanName)) {
				// Not found -> check parent.
				String nameToLookup = originalBeanName(name);
				if (parentBeanFactory instanceof AbstractBeanFactory) {
					return ((AbstractBeanFactory) parentBeanFactory).doGetBean(
							nameToLookup, requiredType, args, typeCheckOnly);
				}
				else if (args != null) {
					// Delegation to parent with explicit args.
					return (T) parentBeanFactory.getBean(nameToLookup, args);
				}
				else if (requiredType != null) {
					// No args -> delegate to standard getBean method.
					return parentBeanFactory.getBean(nameToLookup, requiredType);
				}
				else {
					return (T) parentBeanFactory.getBean(nameToLookup);
				}
			}

      
			if (!typeCheckOnly) {
        //标记bean要被创建，这个相当于创建前的flag，防止并发创建
				markBeanAsCreated(beanName);
			}

			try {
				RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
				checkMergedBeanDefinition(mbd, beanName, args);

				// Guarantee initialization of beans that the current bean depends on.
        //bean是否依赖于其他bean，如果依赖，优先创建其他bean
				String[] dependsOn = mbd.getDependsOn();
				if (dependsOn != null) {
					for (String dep : dependsOn) {
						if (isDependent(beanName, dep)) {
							throw new BeanCreationException(mbd.getResourceDescription(), beanName,
									"Circular depends-on relationship between '" + beanName + "' and '" + dep + "'");
						}
						registerDependentBean(dep, beanName);
						try {
              //存在依赖，优先创建依赖的bean
							getBean(dep);
						}
						catch (NoSuchBeanDefinitionException ex) {
							throw new BeanCreationException(mbd.getResourceDescription(), beanName,
									"'" + beanName + "' depends on missing bean '" + dep + "'", ex);
						}
					}
				}

				// Create bean instance.
        //创建之前，再次校验是否是单例bean，这个之前校验过，这里为了安全再次进行了校验
				if (mbd.isSingleton()) {
					sharedInstance = getSingleton(beanName, () -> {
						try {
							return createBean(beanName, mbd, args);
						}
						catch (BeansException ex) {
							// Explicitly remove instance from singleton cache: It might have been put there
							// eagerly by the creation process, to allow for circular reference resolution.
							// Also remove any beans that received a temporary reference to the bean.
							destroySingleton(beanName);
							throw ex;
						}
					});
					bean = getObjectForBeanInstance(sharedInstance, name, beanName, mbd);
				}

        //如果是非单例bean，创建prototype类型的bean
				else if (mbd.isPrototype()) {
					// It's a prototype -> create a new instance.
					Object prototypeInstance = null;
					try {
						beforePrototypeCreation(beanName);
						prototypeInstance = createBean(beanName, mbd, args);
					}
					finally {
						afterPrototypeCreation(beanName);
					}
					bean = getObjectForBeanInstance(prototypeInstance, name, beanName, mbd);
				}
        //非singleTon 非prototype类型的bean的创建
				else {
					String scopeName = mbd.getScope();
					if (!StringUtils.hasLength(scopeName)) {
						throw new IllegalStateException("No scope name defined for bean ´" + beanName + "'");
					}
					Scope scope = this.scopes.get(scopeName);
					if (scope == null) {
						throw new IllegalStateException("No Scope registered for scope name '" + scopeName + "'");
					}
					try {
						Object scopedInstance = scope.get(beanName, () -> {
							beforePrototypeCreation(beanName);
							try {
								return createBean(beanName, mbd, args);
							}
							finally {
								afterPrototypeCreation(beanName);
							}
						});
						bean = getObjectForBeanInstance(scopedInstance, name, beanName, mbd);
					}
					catch (IllegalStateException ex) {
						throw new BeanCreationException(beanName,
								"Scope '" + scopeName + "' is not active for the current thread; consider " +
								"defining a scoped proxy for this bean if you intend to refer to it from a singleton",
								ex);
					}
				}
			}
			catch (BeansException ex) {
				cleanupAfterBeanCreationFailure(beanName);
				throw ex;
			}
		}

		// Check if required type matches the type of the actual bean instance.
    //检查创建的bean的类型是否为当前class的类型，bean返回之前的 安全校验
		if (requiredType != null && !requiredType.isInstance(bean)) {
			try {
				T convertedBean = getTypeConverter().convertIfNecessary(bean, requiredType);
				if (convertedBean == null) {
					throw new BeanNotOfRequiredTypeException(name, requiredType, bean.getClass());
				}
				return convertedBean;
			}
			catch (TypeMismatchException ex) {
				if (logger.isTraceEnabled()) {
					logger.trace("Failed to convert bean '" + name + "' to required type '" +
							ClassUtils.getQualifiedName(requiredType) + "'", ex);
				}
				throw new BeanNotOfRequiredTypeException(name, requiredType, bean.getClass());
			}
		}
		return (T) bean;
	}

```



#### getSingleton()

是否已经初始化了判断

**org.springframework.beans.factory.support.DefaultSingletonBeanRegistry#getSingleton(java.lang.String, boolean)**

这里也是为了解决循环依赖，创建了3级缓存，关于循环依赖后期单独拿出来讲，这里只提一下，做个标记

![image-20220217153817164](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217153817164.png)





```java
	// Fail if we're already creating this bean instance:
			// We're assumably within a circular reference.
      //改段代码判断是否bean正在被创建，如果被创建，就抛出异常，也是解决循环依赖问题的一项	
			if (isPrototypeCurrentlyInCreation(beanName)) {
				throw new BeanCurrentlyInCreationException(beanName);
			}
```



在factory中是否包含对应的 beanDefinition

```java
// Check if bean definition exists in this factory.
			BeanFactory parentBeanFactory = getParentBeanFactory();
			if (parentBeanFactory != null && !containsBeanDefinition(beanName)) {
				// Not found -> check parent.
				String nameToLookup = originalBeanName(name);
				if (parentBeanFactory instanceof AbstractBeanFactory) {
					return ((AbstractBeanFactory) parentBeanFactory).doGetBean(
							nameToLookup, requiredType, args, typeCheckOnly);
				}
				else if (args != null) {
					// Delegation to parent with explicit args.
					return (T) parentBeanFactory.getBean(nameToLookup, args);
				}
				else if (requiredType != null) {
					// No args -> delegate to standard getBean method.
					return parentBeanFactory.getBean(nameToLookup, requiredType);
				}
				else {
					return (T) parentBeanFactory.getBean(nameToLookup);
				}
			}
```





```java
//防止并发创建，创建开始进行一个标记开始创建了，相当于多状态控制一样
if (!typeCheckOnly) {
				markBeanAsCreated(beanName);
			}

这段代码往里面继续跟踪，
  this.alreadyCreated.add(beanName);核心是这句，如果已经创建会添加
```



如果bean中包含 `@DependsOn` 注解，优先创建依赖

```java
try {
				RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
				checkMergedBeanDefinition(mbd, beanName, args);

				// Guarantee initialization of beans that the current bean depends on.
				String[] dependsOn = mbd.getDependsOn();
				if (dependsOn != null) {
					for (String dep : dependsOn) {
						if (isDependent(beanName, dep)) {
							throw new BeanCreationException(mbd.getResourceDescription(), beanName,
									"Circular depends-on relationship between '" + beanName + "' and '" + dep + "'");
						}
						registerDependentBean(dep, beanName);
						try {
							getBean(dep);
						}
						catch (NoSuchBeanDefinitionException ex) {
							throw new BeanCreationException(mbd.getResourceDescription(), beanName,
									"'" + beanName + "' depends on missing bean '" + dep + "'", ex);
						}
					}
				}

```



#### 真正开始创建bean

首先进行前置条件判断，判断是否是单例bean

```java
// Create bean instance.
				if (mbd.isSingleton()) {
					sharedInstance = getSingleton(beanName, () -> {
						try {
							return createBean(beanName, mbd, args);
						}
						catch (BeansException ex) {
							// Explicitly remove instance from singleton cache: It might have been put there
							// eagerly by the creation process, to allow for circular reference resolution.
							// Also remove any beans that received a temporary reference to the bean.
							destroySingleton(beanName);
							throw ex;
						}
					});
					bean = getObjectForBeanInstance(sharedInstance, name, beanName, mbd);
				}
```



#### DefaultSingletonBeanRegistry#getSingleton

`org.springframework.beans.factory.support.DefaultSingletonBeanRegistry#getSingleton(java.lang.String, org.springframework.beans.factory.ObjectFactory<?>)`

```java
/**
	 * Return the (raw) singleton object registered under the given name,
	 * creating and registering a new one if none registered yet.
	 * @param beanName the name of the bean
	 * @param singletonFactory the ObjectFactory to lazily create the singleton
	 * with, if necessary
	 * @return the registered singleton object
	 */
	public Object getSingleton(String beanName, ObjectFactory<?> singletonFactory) {
		Assert.notNull(beanName, "Bean name must not be null");
		synchronized (this.singletonObjects) {
      //先判断singletonObjects中，是否有，如果有，直接返回，否则继续创建
			Object singletonObject = this.singletonObjects.get(beanName);
			if (singletonObject == null) {
        //再次检查是否正在被创建
				if (this.singletonsCurrentlyInDestruction) {
					throw new BeanCreationNotAllowedException(beanName,
							"Singleton bean creation not allowed while singletons of this factory are in destruction " +
							"(Do not request a bean from a BeanFactory in a destroy method implementation!)");
				}
				if (logger.isDebugEnabled()) {
					logger.debug("Creating shared instance of singleton bean '" + beanName + "'");
				}
        //创建前的准备工作，还是为了循环引用准备，是否正在创建
				beforeSingletonCreation(beanName);
				boolean newSingleton = false;
				boolean recordSuppressedExceptions = (this.suppressedExceptions == null);
				if (recordSuppressedExceptions) {
					this.suppressedExceptions = new LinkedHashSet<>();
				}
				try {
          //开始创建bean
					singletonObject = singletonFactory.getObject();
					newSingleton = true;
				}
				catch (IllegalStateException ex) {
					// Has the singleton object implicitly appeared in the meantime ->
					// if yes, proceed with it since the exception indicates that state.
					singletonObject = this.singletonObjects.get(beanName);
					if (singletonObject == null) {
						throw ex;
					}
				}
				catch (BeanCreationException ex) {
					if (recordSuppressedExceptions) {
						for (Exception suppressedException : this.suppressedExceptions) {
							ex.addRelatedCause(suppressedException);
						}
					}
					throw ex;
				}
				finally {
					if (recordSuppressedExceptions) {
						this.suppressedExceptions = null;
					}
					afterSingletonCreation(beanName);
				}
				if (newSingleton) {
          //添加到singleTonObjects中
					addSingleton(beanName, singletonObject);
				}
			}
			return singletonObject;
		}
	}
```



#### 回到创建bean

这里传入的`createBean(beanName, mbd, args);`就是开始创建`bean`

```java
if (mbd.isSingleton()) {
					sharedInstance = getSingleton(beanName, () -> {
						try {
							return createBean(beanName, mbd, args);
						}
						catch (BeansException ex) {
							// Explicitly remove instance from singleton cache: It might have been put there
							// eagerly by the creation process, to allow for circular reference resolution.
							// Also remove any beans that received a temporary reference to the bean.
							destroySingleton(beanName);
							throw ex;
						}
					});
					bean = getObjectForBeanInstance(sharedInstance, name, beanName, mbd);
				}

```



![image-20220217161014306](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217161014306.png)





![image-20220217160659429](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217160659429.png)



#### AbstractAutowireCapableBeanFactory#createBean

`org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory#createBean(java.lang.String, org.springframework.beans.factory.support.RootBeanDefinition, java.lang.Object[])`



```java
/**
	 * Central method of this class: creates a bean instance,
	 * populates the bean instance, applies post-processors, etc.
	 * @see #doCreateBean
	 */
	@Override
	protected Object createBean(String beanName, RootBeanDefinition mbd, @Nullable Object[] args)
			throws BeanCreationException {

		if (logger.isTraceEnabled()) {
			logger.trace("Creating instance of bean '" + beanName + "'");
		}
		RootBeanDefinition mbdToUse = mbd;

		// Make sure bean class is actually resolved at this point, and
		// clone the bean definition in case of a dynamically resolved Class
		// which cannot be stored in the shared merged bean definition.
    //获取bean的class定义信息
		Class<?> resolvedClass = resolveBeanClass(mbd, beanName);
		if (resolvedClass != null && !mbd.hasBeanClass() && mbd.getBeanClassName() != null) {
			mbdToUse = new RootBeanDefinition(mbd);
			mbdToUse.setBeanClass(resolvedClass);
		}

		// Prepare method overrides.
    //方法是否被重写
		try {
			mbdToUse.prepareMethodOverrides();
		}
		catch (BeanDefinitionValidationException ex) {
			throw new BeanDefinitionStoreException(mbdToUse.getResourceDescription(),
					beanName, "Validation of method overrides failed", ex);
		}

		try {
      //如果需要创建代理对象
      // 通过BeanPostProcessor获取代理增强的对象，这块和AOP联系起来了，分析IOC先放下
			// Give BeanPostProcessors a chance to return a proxy instead of the target bean instance.
			Object bean = resolveBeforeInstantiation(beanName, mbdToUse);
			if (bean != null) {
				return bean;
			}
		}
		catch (Throwable ex) {
			throw new BeanCreationException(mbdToUse.getResourceDescription(), beanName,
					"BeanPostProcessor before instantiation of bean failed", ex);
		}

    //不需要创建代理对象，直接创建对象
		try {
      //直接创建对象
			Object beanInstance = doCreateBean(beanName, mbdToUse, args);
			if (logger.isTraceEnabled()) {
				logger.trace("Finished creating instance of bean '" + beanName + "'");
			}
			return beanInstance;
		}
		catch (BeanCreationException | ImplicitlyAppearedSingletonException ex) {
			// A previously detected exception with proper bean creation context already,
			// or illegal singleton state to be communicated up to DefaultSingletonBeanRegistry.
			throw ex;
		}
		catch (Throwable ex) {
			throw new BeanCreationException(
					mbdToUse.getResourceDescription(), beanName, "Unexpected exception during bean creation", ex);
		}
	}

	/**
	 * Actually create the specified bean. Pre-creation processing has already happened
	 * at this point, e.g. checking {@code postProcessBeforeInstantiation} callbacks.
	 * <p>Differentiates between default bean instantiation, use of a
	 * factory method, and autowiring a constructor.
	 * @param beanName the name of the bean
	 * @param mbd the merged bean definition for the bean
	 * @param args explicit arguments to use for constructor or factory method invocation
	 * @return a new instance of the bean
	 * @throws BeanCreationException if the bean could not be created
	 * @see #instantiateBean
	 * @see #instantiateUsingFactoryMethod
	 * @see #autowireConstructor
	 */
	protected Object doCreateBean(String beanName, RootBeanDefinition mbd, @Nullable Object[] args)
			throws BeanCreationException {

		// Instantiate the bean.
		BeanWrapper instanceWrapper = null;
		if (mbd.isSingleton()) {
			instanceWrapper = this.factoryBeanInstanceCache.remove(beanName);
		}
		if (instanceWrapper == null) {
			instanceWrapper = createBeanInstance(beanName, mbd, args);
		}
		Object bean = instanceWrapper.getWrappedInstance();
		Class<?> beanType = instanceWrapper.getWrappedClass();
		if (beanType != NullBean.class) {
			mbd.resolvedTargetType = beanType;
		}

		// Allow post-processors to modify the merged bean definition.
		synchronized (mbd.postProcessingLock) {
			if (!mbd.postProcessed) {
				try {
					applyMergedBeanDefinitionPostProcessors(mbd, beanType, beanName);
				}
				catch (Throwable ex) {
					throw new BeanCreationException(mbd.getResourceDescription(), beanName,
							"Post-processing of merged bean definition failed", ex);
				}
				mbd.postProcessed = true;
			}
		}

		// Eagerly cache singletons to be able to resolve circular references
		// even when triggered by lifecycle interfaces like BeanFactoryAware.
		boolean earlySingletonExposure = (mbd.isSingleton() && this.allowCircularReferences &&
				isSingletonCurrentlyInCreation(beanName));
		if (earlySingletonExposure) {
			if (logger.isTraceEnabled()) {
				logger.trace("Eagerly caching bean '" + beanName +
						"' to allow for resolving potential circular references");
			}
			addSingletonFactory(beanName, () -> getEarlyBeanReference(beanName, mbd, bean));
		}

		// Initialize the bean instance.
		Object exposedObject = bean;
		try {
			populateBean(beanName, mbd, instanceWrapper);
			exposedObject = initializeBean(beanName, exposedObject, mbd);
		}
		catch (Throwable ex) {
			if (ex instanceof BeanCreationException && beanName.equals(((BeanCreationException) ex).getBeanName())) {
				throw (BeanCreationException) ex;
			}
			else {
				throw new BeanCreationException(
						mbd.getResourceDescription(), beanName, "Initialization of bean failed", ex);
			}
		}

		if (earlySingletonExposure) {
			Object earlySingletonReference = getSingleton(beanName, false);
			if (earlySingletonReference != null) {
				if (exposedObject == bean) {
					exposedObject = earlySingletonReference;
				}
				else if (!this.allowRawInjectionDespiteWrapping && hasDependentBean(beanName)) {
					String[] dependentBeans = getDependentBeans(beanName);
					Set<String> actualDependentBeans = new LinkedHashSet<>(dependentBeans.length);
					for (String dependentBean : dependentBeans) {
						if (!removeSingletonIfCreatedForTypeCheckOnly(dependentBean)) {
							actualDependentBeans.add(dependentBean);
						}
					}
					if (!actualDependentBeans.isEmpty()) {
						throw new BeanCurrentlyInCreationException(beanName,
								"Bean with name '" + beanName + "' has been injected into other beans [" +
								StringUtils.collectionToCommaDelimitedString(actualDependentBeans) +
								"] in its raw version as part of a circular reference, but has eventually been " +
								"wrapped. This means that said other beans do not use the final version of the " +
								"bean. This is often the result of over-eager type matching - consider using " +
								"'getBeanNamesForType' with the 'allowEagerInit' flag turned off, for example.");
					}
				}
			}
		}

		// Register bean as disposable.
		try {
			registerDisposableBeanIfNecessary(beanName, bean, mbd);
		}
		catch (BeanDefinitionValidationException ex) {
			throw new BeanCreationException(
					mbd.getResourceDescription(), beanName, "Invalid destruction signature", ex);
		}

		return exposedObject;
	}

```





![image-20220217161238656](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217161238656.png)



#### 关于动态代理创建AbstractAutowireCapableBeanFactory#resolveBeforeInstantiation



org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory#resolveBeforeInstantiation

关于动态代理后期会重点看第一个类，是一个`BeanPostProcessor` 就是`AbstractAutoProxyCreator``

![image-20220217162525105](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217162525105.png)



#### InstantiationAwareBeanPostProcessor

看这个类的实现类，是真正的AOP入口，后期研究AOP时，会碰到



这里先做个标记

```java
public abstract class AbstractAdvisorAutoProxyCreator extends AbstractAutoProxyCreator {

  public abstract class AbstractAutoProxyCreator extends ProxyProcessorSupport
		implements SmartInstantiationAwareBeanPostProcessor, BeanFactoryAware {

  public class ProxyProcessorSupport extends ProxyConfig implements Ordered, BeanClassLoaderAware, AopInfrastructureBean {
    
public interface SmartInstantiationAwareBeanPostProcessor extends InstantiationAwareBeanPostProcessor {
  
  
public interface InstantiationAwareBeanPostProcessor extends BeanPostProcessor {
 
    
```





![image-20220217162956465](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217162956465.png)



### 继续AbstractAutowireCapableBeanFactory.doCreateBean

`org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory#doCreateBean`



```java
/**
	 * Actually create the specified bean. Pre-creation processing has already happened
	 * at this point, e.g. checking {@code postProcessBeforeInstantiation} callbacks.
	 * <p>Differentiates between default bean instantiation, use of a
	 * factory method, and autowiring a constructor.
	 * @param beanName the name of the bean
	 * @param mbd the merged bean definition for the bean
	 * @param args explicit arguments to use for constructor or factory method invocation
	 * @return a new instance of the bean
	 * @throws BeanCreationException if the bean could not be created
	 * @see #instantiateBean
	 * @see #instantiateUsingFactoryMethod
	 * @see #autowireConstructor
	 */
	protected Object doCreateBean(String beanName, RootBeanDefinition mbd, @Nullable Object[] args)
			throws BeanCreationException {

		// Instantiate the bean.
		BeanWrapper instanceWrapper = null;
		if (mbd.isSingleton()) {
			instanceWrapper = this.factoryBeanInstanceCache.remove(beanName);
		}
		if (instanceWrapper == null) {
      //创建BeanWrapper，这是经常的手法，创建一个XXXXWraper，或者XXXXHolder 用于封装对应的 bean
			instanceWrapper = createBeanInstance(beanName, mbd, args);
		}
		Object bean = instanceWrapper.getWrappedInstance();
		Class<?> beanType = instanceWrapper.getWrappedClass();
		if (beanType != NullBean.class) {
			mbd.resolvedTargetType = beanType;
		}

		// Allow post-processors to modify the merged bean definition.
   // 在创建前可以通过MergedBeanDefinitionPostProcessor 做最后的修改  相当于修改BeanDefinition的扩展点
		synchronized (mbd.postProcessingLock) {
			if (!mbd.postProcessed) {
				try {
					applyMergedBeanDefinitionPostProcessors(mbd, beanType, beanName);
				}
				catch (Throwable ex) {
					throw new BeanCreationException(mbd.getResourceDescription(), beanName,
							"Post-processing of merged bean definition failed", ex);
				}
				mbd.postProcessed = true;
			}
		}

		// Eagerly cache singletons to be able to resolve circular references
		// even when triggered by lifecycle interfaces like BeanFactoryAware.
    //为了解决循环引用问题，提前声明的缓存，缓存单例对象
		boolean earlySingletonExposure = (mbd.isSingleton() && this.allowCircularReferences &&
				isSingletonCurrentlyInCreation(beanName));
		if (earlySingletonExposure) {
			if (logger.isTraceEnabled()) {
				logger.trace("Eagerly caching bean '" + beanName +
						"' to allow for resolving potential circular references");
			}
			addSingletonFactory(beanName, () -> getEarlyBeanReference(beanName, mbd, bean));
		}

		// Initialize the bean instance.
		Object exposedObject = bean;
		try {
      //属性赋值和自动注入
			populateBean(beanName, mbd, instanceWrapper);
			exposedObject = initializeBean(beanName, exposedObject, mbd);
		}
		catch (Throwable ex) {
			if (ex instanceof BeanCreationException && beanName.equals(((BeanCreationException) ex).getBeanName())) {
				throw (BeanCreationException) ex;
			}
			else {
				throw new BeanCreationException(
						mbd.getResourceDescription(), beanName, "Initialization of bean failed", ex);
			}
		}

    //创建好单例bean，但未放入缓存中，第一次跳过
		if (earlySingletonExposure) {
			Object earlySingletonReference = getSingleton(beanName, false);
			if (earlySingletonReference != null) {
				if (exposedObject == bean) {
					exposedObject = earlySingletonReference;
				}
				else if (!this.allowRawInjectionDespiteWrapping && hasDependentBean(beanName)) {
					String[] dependentBeans = getDependentBeans(beanName);
					Set<String> actualDependentBeans = new LinkedHashSet<>(dependentBeans.length);
					for (String dependentBean : dependentBeans) {
						if (!removeSingletonIfCreatedForTypeCheckOnly(dependentBean)) {
							actualDependentBeans.add(dependentBean);
						}
					}
					if (!actualDependentBeans.isEmpty()) {
						throw new BeanCurrentlyInCreationException(beanName,
								"Bean with name '" + beanName + "' has been injected into other beans [" +
								StringUtils.collectionToCommaDelimitedString(actualDependentBeans) +
								"] in its raw version as part of a circular reference, but has eventually been " +
								"wrapped. This means that said other beans do not use the final version of the " +
								"bean. This is often the result of over-eager type matching - consider using " +
								"'getBeanNamesForType' with the 'allowEagerInit' flag turned off, for example.");
					}
				}
			}
		}

		// Register bean as disposable.
    //注册注销钩子，容器销毁时，销毁bean
		try {
			registerDisposableBeanIfNecessary(beanName, bean, mbd);
		}
		catch (BeanDefinitionValidationException ex) {
			throw new BeanCreationException(
					mbd.getResourceDescription(), beanName, "Invalid destruction signature", ex);
		}

		return exposedObject;
	}

```



#### AbstractAutowireCapableBeanFactory#createBeanInstance

`org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory#createBeanInstance`

调用创建bean



```java
/**
	 * Create a new instance for the specified bean, using an appropriate instantiation strategy:
	 * factory method, constructor autowiring, or simple instantiation.
	 * @param beanName the name of the bean
	 * @param mbd the bean definition for the bean
	 * @param args explicit arguments to use for constructor or factory method invocation
	 * @return a BeanWrapper for the new instance
	 * @see #obtainFromSupplier
	 * @see #instantiateUsingFactoryMethod
	 * @see #autowireConstructor
	 * @see #instantiateBean
	 */
	protected BeanWrapper createBeanInstance(String beanName, RootBeanDefinition mbd, @Nullable Object[] args) {
		// Make sure bean class is actually resolved at this point.
    //获取对应的class信息
		Class<?> beanClass = resolveBeanClass(mbd, beanName);

		if (beanClass != null && !Modifier.isPublic(beanClass.getModifiers()) && !mbd.isNonPublicAccessAllowed()) {
			throw new BeanCreationException(mbd.getResourceDescription(), beanName,
					"Bean class isn't public, and non-public access not allowed: " + beanClass.getName());
		}

    //是否有回调，如果有，通过回调处理创建对象，一种包含回调，单独创建对象的方式
    //深入分析用于test事，创建对象，这里跳过处理
		Supplier<?> instanceSupplier = mbd.getInstanceSupplier();
		if (instanceSupplier != null) {
			return obtainFromSupplier(instanceSupplier, beanName);
		}

    //如果存在工厂方法，直接调用工厂方法创建bean
		if (mbd.getFactoryMethodName() != null) {
			return instantiateUsingFactoryMethod(beanName, mbd, args);
		}

		// Shortcut when re-creating the same bean...
    //当创建同样对象时，再次调用
		boolean resolved = false;
		boolean autowireNecessary = false;
		if (args == null) {
			synchronized (mbd.constructorArgumentLock) {
				if (mbd.resolvedConstructorOrFactoryMethod != null) {
					resolved = true;
          //确定构造方法
					autowireNecessary = mbd.constructorArgumentsResolved;
				}
			}
		}
		if (resolved) {
			if (autowireNecessary) {
				return autowireConstructor(beanName, mbd, null, null);
			}
			else {
				return instantiateBean(beanName, mbd);
			}
		}
    
    //选取构造方法

		// Candidate constructors for autowiring?
		Constructor<?>[] ctors = determineConstructorsFromBeanPostProcessors(beanClass, beanName);
		if (ctors != null || mbd.getResolvedAutowireMode() == AUTOWIRE_CONSTRUCTOR ||
				mbd.hasConstructorArgumentValues() || !ObjectUtils.isEmpty(args)) {
			return autowireConstructor(beanName, mbd, ctors, args);
		}
    //是否有合适的构造，如果有通过对应构造创建对象
		// Preferred constructors for default construction?
		ctors = mbd.getPreferredConstructors();
		if (ctors != null) {
			return autowireConstructor(beanName, mbd, ctors, null);
		}
    
    //如果没有最后会用默认的无参构造方法创建对象
		// No special handling: simply use no-arg constructor.
		return instantiateBean(beanName, mbd);
	}
```



关于如何解析对应的 构造方法，后期单独分析，这里主线是大部分构造是默认的构造，直接调到最后一行，利用默认构造方法去创建对象

![image-20220217165256593](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217165256593.png)



#### instantiateBean(beanName, mbd)

`org.springframework.beans.factory.support.SimpleInstantiationStrategy#instantiate(org.springframework.beans.factory.support.RootBeanDefinition, java.lang.String, org.springframework.beans.factory.BeanFactory)`



```java
@Override
	public Object instantiate(RootBeanDefinition bd, @Nullable String beanName, BeanFactory owner) {
		// Don't override the class with CGLIB if no overrides.
		if (!bd.hasMethodOverrides()) {
			Constructor<?> constructorToUse;
			synchronized (bd.constructorArgumentLock) {
				constructorToUse = (Constructor<?>) bd.resolvedConstructorOrFactoryMethod;
				if (constructorToUse == null) {
					final Class<?> clazz = bd.getBeanClass();
					if (clazz.isInterface()) {
						throw new BeanInstantiationException(clazz, "Specified class is an interface");
					}
					try {
						if (System.getSecurityManager() != null) {
							constructorToUse = AccessController.doPrivileged(
									(PrivilegedExceptionAction<Constructor<?>>) clazz::getDeclaredConstructor);
						}
						else {
							constructorToUse = clazz.getDeclaredConstructor();
						}
						bd.resolvedConstructorOrFactoryMethod = constructorToUse;
					}
					catch (Throwable ex) {
						throw new BeanInstantiationException(clazz, "No default constructor found", ex);
					}
				}
			}
			return BeanUtils.instantiateClass(constructorToUse);
		}
		else {
			// Must generate CGLIB subclass.
			return instantiateWithMethodInjection(bd, beanName, owner);
		}
	}
```



![image-20220217165500061](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217165500061.png)



#### org.springframework.beans.BeanUtils#instantiateClass(java.lang.reflect.Constructor<T>, java.lang.Object...)



```java
/**
	 * Convenience method to instantiate a class using the given constructor.
	 * <p>Note that this method tries to set the constructor accessible if given a
	 * non-accessible (that is, non-public) constructor, and supports Kotlin classes
	 * with optional parameters and default values.
	 * @param ctor the constructor to instantiate
	 * @param args the constructor arguments to apply (use {@code null} for an unspecified
	 * parameter, Kotlin optional parameters and Java primitive types are supported)
	 * @return the new instance
	 * @throws BeanInstantiationException if the bean cannot be instantiated
	 * @see Constructor#newInstance
	 */
	public static <T> T instantiateClass(Constructor<T> ctor, Object... args) throws BeanInstantiationException {
		Assert.notNull(ctor, "Constructor must not be null");
		try {
			ReflectionUtils.makeAccessible(ctor);
			if (KotlinDetector.isKotlinReflectPresent() && KotlinDetector.isKotlinType(ctor.getDeclaringClass())) {
				return KotlinDelegate.instantiateClass(ctor, args);
			}
			else {
				Class<?>[] parameterTypes = ctor.getParameterTypes();
				Assert.isTrue(args.length <= parameterTypes.length, "Can't specify more arguments than constructor parameters");
				Object[] argsWithDefaultValues = new Object[args.length];
				for (int i = 0 ; i < args.length; i++) {
					if (args[i] == null) {
						Class<?> parameterType = parameterTypes[i];
						argsWithDefaultValues[i] = (parameterType.isPrimitive() ? DEFAULT_TYPE_VALUES.get(parameterType) : null);
					}
					else {
						argsWithDefaultValues[i] = args[i];
					}
				}
				return ctor.newInstance(argsWithDefaultValues);
			}
		}
		catch (InstantiationException ex) {
			throw new BeanInstantiationException(ctor, "Is it an abstract class?", ex);
		}
		catch (IllegalAccessException ex) {
			throw new BeanInstantiationException(ctor, "Is the constructor accessible?", ex);
		}
		catch (IllegalArgumentException ex) {
			throw new BeanInstantiationException(ctor, "Illegal arguments for constructor", ex);
		}
		catch (InvocationTargetException ex) {
			throw new BeanInstantiationException(ctor, "Constructor threw exception", ex.getTargetException());
		}
	}
```

构造创建如下： 

![image-20220217165601888](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217165601888.png)



创建对象： 

![image-20220217165737237](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217165737237.png)



### 开始初始化属性



#### populateBean



```java
	// Initialize the bean instance.
		Object exposedObject = bean;
		try {
			populateBean(beanName, mbd, instanceWrapper);
			exposedObject = initializeBean(beanName, exposedObject, mbd);
		}
		catch (Throwable ex) {
			if (ex instanceof BeanCreationException && beanName.equals(((BeanCreationException) ex).getBeanName())) {
				throw (BeanCreationException) ex;
			}
			else {
				throw new BeanCreationException(
						mbd.getResourceDescription(), beanName, "Initialization of bean failed", ex);
			}
		}

		if (earlySingletonExposure) {
			Object earlySingletonReference = getSingleton(beanName, false);
			if (earlySingletonReference != null) {
				if (exposedObject == bean) {
					exposedObject = earlySingletonReference;
				}
				else if (!this.allowRawInjectionDespiteWrapping && hasDependentBean(beanName)) {
					String[] dependentBeans = getDependentBeans(beanName);
					Set<String> actualDependentBeans = new LinkedHashSet<>(dependentBeans.length);
					for (String dependentBean : dependentBeans) {
						if (!removeSingletonIfCreatedForTypeCheckOnly(dependentBean)) {
							actualDependentBeans.add(dependentBean);
						}
					}
					if (!actualDependentBeans.isEmpty()) {
						throw new BeanCurrentlyInCreationException(beanName,
								"Bean with name '" + beanName + "' has been injected into other beans [" +
								StringUtils.collectionToCommaDelimitedString(actualDependentBeans) +
								"] in its raw version as part of a circular reference, but has eventually been " +
								"wrapped. This means that said other beans do not use the final version of the " +
								"bean. This is often the result of over-eager type matching - consider using " +
								"'getBeanNamesForType' with the 'allowEagerInit' flag turned off, for example.");
					}
				}
			}
		}

		// Register bean as disposable.
		try {
			registerDisposableBeanIfNecessary(beanName, bean, mbd);
		}
		catch (BeanDefinitionValidationException ex) {
			throw new BeanCreationException(
					mbd.getResourceDescription(), beanName, "Invalid destruction signature", ex);
		}

		return exposedObject;
	}
```







![image-20220217165953964](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217165953964.png)





#### AbstractAutowireCapableBeanFactory#populateBean



`org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory#populateBean`

```java
protected void populateBean(String beanName, RootBeanDefinition mbd, @Nullable BeanWrapper bw) {
		if (bw == null) {
			if (mbd.hasPropertyValues()) {
				throw new BeanCreationException(
						mbd.getResourceDescription(), beanName, "Cannot apply property values to null instance");
			}
			else {
				// Skip property population phase for null instance.
				return;
			}
		}

		// Give any InstantiationAwareBeanPostProcessors the opportunity to modify the
		// state of the bean before properties are set. This can be used, for example,
		// to support styles of field injection.
//  InstantiationAwareBeanPostProcessor的beanPostProcessor执行
		if (!mbd.isSynthetic() && hasInstantiationAwareBeanPostProcessors()) {
			for (BeanPostProcessor bp : getBeanPostProcessors()) {
				if (bp instanceof InstantiationAwareBeanPostProcessor) {
					InstantiationAwareBeanPostProcessor ibp = (InstantiationAwareBeanPostProcessor) bp;
					if (!ibp.postProcessAfterInstantiation(bw.getWrappedInstance(), beanName)) {
						return;
					}
				}
			}
		}

		PropertyValues pvs = (mbd.hasPropertyValues() ? mbd.getPropertyValues() : null);

		int resolvedAutowireMode = mbd.getResolvedAutowireMode();
		if (resolvedAutowireMode == AUTOWIRE_BY_NAME || resolvedAutowireMode == AUTOWIRE_BY_TYPE) {
			MutablePropertyValues newPvs = new MutablePropertyValues(pvs);
			// Add property values based on autowire by name if applicable.
			if (resolvedAutowireMode == AUTOWIRE_BY_NAME) {
				autowireByName(beanName, mbd, bw, newPvs);
			}
			// Add property values based on autowire by type if applicable.
			if (resolvedAutowireMode == AUTOWIRE_BY_TYPE) {
				autowireByType(beanName, mbd, bw, newPvs);
			}
			pvs = newPvs;
		}

		boolean hasInstAwareBpps = hasInstantiationAwareBeanPostProcessors();
		boolean needsDepCheck = (mbd.getDependencyCheck() != AbstractBeanDefinition.DEPENDENCY_CHECK_NONE);

		PropertyDescriptor[] filteredPds = null;
		if (hasInstAwareBpps) {
			if (pvs == null) {
				pvs = mbd.getPropertyValues();
			}
			for (BeanPostProcessor bp : getBeanPostProcessors()) {
				if (bp instanceof InstantiationAwareBeanPostProcessor) {
           // 执行所有InstantiationAwareBeanPostProcessor的postProcessProperties方法 一些 @Inject @Autowired、@Value 注入
					InstantiationAwareBeanPostProcessor ibp = (InstantiationAwareBeanPostProcessor) bp;
					PropertyValues pvsToUse = ibp.postProcessProperties(pvs, bw.getWrappedInstance(), beanName);
					if (pvsToUse == null) {
						if (filteredPds == null) {
							filteredPds = filterPropertyDescriptorsForDependencyCheck(bw, mbd.allowCaching);
						}
						pvsToUse = ibp.postProcessPropertyValues(pvs, filteredPds, bw.getWrappedInstance(), beanName);
						if (pvsToUse == null) {
							return;
						}
					}
					pvs = pvsToUse;
				}
			}
		}
		if (needsDepCheck) {
			if (filteredPds == null) {
				filteredPds = filterPropertyDescriptorsForDependencyCheck(bw, mbd.allowCaching);
			}
			checkDependencies(beanName, mbd, filteredPds, pvs);
		}

		if (pvs != null) {
			applyPropertyValues(beanName, mbd, bw, pvs);
		}
	}
```

#### 执行所有的InstantiationAwareBeanPostProcessor

![image-20220217170443033](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217170443033.png)



restConfig 没有属性值，直接退出

![image-20220217170943957](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217170943957.png)



### 重点InstantiationAwareBeanPostProcessor

这里的InstantiationAwareBeanPostProcessor集成关系如下：  为什么@Autowire 和 @Inject在这里会进行属性注入呢？？？？

看下如下集成关系： 

```java
public class AutowiredAnnotationBeanPostProcessor extends InstantiationAwareBeanPostProcessorAdapter
		implements MergedBeanDefinitionPostProcessor, PriorityOrdered, BeanFactoryAware {
  

  public abstract class InstantiationAwareBeanPostProcessorAdapter implements SmartInstantiationAwareBeanPostProcessor {

    public interface SmartInstantiationAwareBeanPostProcessor extends InstantiationAwareBeanPostProcessor {

      public interface InstantiationAwareBeanPostProcessor extends BeanPostProcessor {

```



#### InstantiationAwareBeanPostProcessor 核心定义如下： 

```java
/*
 * Copyright 2002-2019 the original author or authors.
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

package org.springframework.beans.factory.config;

import java.beans.PropertyDescriptor;

import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValues;
import org.springframework.lang.Nullable;

/**
 * Subinterface of {@link BeanPostProcessor} that adds a before-instantiation callback,
 * and a callback after instantiation but before explicit properties are set or
 * autowiring occurs.
 *
 * <p>Typically used to suppress default instantiation for specific target beans,
 * for example to create proxies with special TargetSources (pooling targets,
 * lazily initializing targets, etc), or to implement additional injection strategies
 * such as field injection.
 *
 * <p><b>NOTE:</b> This interface is a special purpose interface, mainly for
 * internal use within the framework. It is recommended to implement the plain
 * {@link BeanPostProcessor} interface as far as possible, or to derive from
 * {@link InstantiationAwareBeanPostProcessorAdapter} in order to be shielded
 * from extensions to this interface.
 *
 * @author Juergen Hoeller
 * @author Rod Johnson
 * @since 1.2
 * @see org.springframework.aop.framework.autoproxy.AbstractAutoProxyCreator#setCustomTargetSourceCreators
 * @see org.springframework.aop.framework.autoproxy.target.LazyInitTargetSourceCreator
 */
public interface InstantiationAwareBeanPostProcessor extends BeanPostProcessor {

	/**
	 * Apply this BeanPostProcessor <i>before the target bean gets instantiated</i>.
	 * The returned bean object may be a proxy to use instead of the target bean,
	 * effectively suppressing default instantiation of the target bean.
	 * <p>If a non-null object is returned by this method, the bean creation process
	 * will be short-circuited. The only further processing applied is the
	 * {@link #postProcessAfterInitialization} callback from the configured
	 * {@link BeanPostProcessor BeanPostProcessors}.
	 * <p>This callback will be applied to bean definitions with their bean class,
	 * as well as to factory-method definitions in which case the returned bean type
	 * will be passed in here.
	 * <p>Post-processors may implement the extended
	 * {@link SmartInstantiationAwareBeanPostProcessor} interface in order
	 * to predict the type of the bean object that they are going to return here.
	 * <p>The default implementation returns {@code null}.
	 * @param beanClass the class of the bean to be instantiated
	 * @param beanName the name of the bean
	 * @return the bean object to expose instead of a default instance of the target bean,
	 * or {@code null} to proceed with default instantiation
	 * @throws org.springframework.beans.BeansException in case of errors
	 * @see #postProcessAfterInstantiation
	 * @see org.springframework.beans.factory.support.AbstractBeanDefinition#getBeanClass()
	 * @see org.springframework.beans.factory.support.AbstractBeanDefinition#getFactoryMethodName()
	 */
	@Nullable
	default Object postProcessBeforeInstantiation(Class<?> beanClass, String beanName) throws BeansException {
		return null;
	}

	/**
	 * Perform operations after the bean has been instantiated, via a constructor or factory method,
	 * but before Spring property population (from explicit properties or autowiring) occurs.
	 * <p>This is the ideal callback for performing custom field injection on the given bean
	 * instance, right before Spring's autowiring kicks in.
	 * <p>The default implementation returns {@code true}.
	 * @param bean the bean instance created, with properties not having been set yet
	 * @param beanName the name of the bean
	 * @return {@code true} if properties should be set on the bean; {@code false}
	 * if property population should be skipped. Normal implementations should return {@code true}.
	 * Returning {@code false} will also prevent any subsequent InstantiationAwareBeanPostProcessor
	 * instances being invoked on this bean instance.
	 * @throws org.springframework.beans.BeansException in case of errors
	 * @see #postProcessBeforeInstantiation
	 */
	default boolean postProcessAfterInstantiation(Object bean, String beanName) throws BeansException {
		return true;
	}

	/**
	 * Post-process the given property values before the factory applies them
	 * to the given bean, without any need for property descriptors.
	 * <p>Implementations should return {@code null} (the default) if they provide a custom
	 * {@link #postProcessPropertyValues} implementation, and {@code pvs} otherwise.
	 * In a future version of this interface (with {@link #postProcessPropertyValues} removed),
	 * the default implementation will return the given {@code pvs} as-is directly.
	 * @param pvs the property values that the factory is about to apply (never {@code null})
	 * @param bean the bean instance created, but whose properties have not yet been set
	 * @param beanName the name of the bean
	 * @return the actual property values to apply to the given bean (can be the passed-in
	 * PropertyValues instance), or {@code null} which proceeds with the existing properties
	 * but specifically continues with a call to {@link #postProcessPropertyValues}
	 * (requiring initialized {@code PropertyDescriptor}s for the current bean class)
	 * @throws org.springframework.beans.BeansException in case of errors
	 * @since 5.1
	 * @see #postProcessPropertyValues
	 */
	@Nullable
	default PropertyValues postProcessProperties(PropertyValues pvs, Object bean, String beanName)
			throws BeansException {

		return null;
	}

	/**
	 * Post-process the given property values before the factory applies them
	 * to the given bean. Allows for checking whether all dependencies have been
	 * satisfied, for example based on a "Required" annotation on bean property setters.
	 * <p>Also allows for replacing the property values to apply, typically through
	 * creating a new MutablePropertyValues instance based on the original PropertyValues,
	 * adding or removing specific values.
	 * <p>The default implementation returns the given {@code pvs} as-is.
	 * @param pvs the property values that the factory is about to apply (never {@code null})
	 * @param pds the relevant property descriptors for the target bean (with ignored
	 * dependency types - which the factory handles specifically - already filtered out)
	 * @param bean the bean instance created, but whose properties have not yet been set
	 * @param beanName the name of the bean
	 * @return the actual property values to apply to the given bean (can be the passed-in
	 * PropertyValues instance), or {@code null} to skip property population
	 * @throws org.springframework.beans.BeansException in case of errors
	 * @see #postProcessProperties
	 * @see org.springframework.beans.MutablePropertyValues
	 * @deprecated as of 5.1, in favor of {@link #postProcessProperties(PropertyValues, Object, String)}
	 */
	@Deprecated
	@Nullable
	default PropertyValues postProcessPropertyValues(
			PropertyValues pvs, PropertyDescriptor[] pds, Object bean, String beanName) throws BeansException {

		return pvs;
	}

}

```



看下这几个实现，大概能够明白，如何实现的了比如`org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor#postProcessProperties`



@Inject

```java
@Override
	public PropertyValues postProcessProperties(PropertyValues pvs, Object bean, String beanName) {
		InjectionMetadata metadata = findAutowiringMetadata(beanName, bean.getClass(), pvs);
		try {
			metadata.inject(bean, beanName, pvs);
		}
		catch (BeanCreationException ex) {
			throw ex;
		}
		catch (Throwable ex) {
			throw new BeanCreationException(beanName, "Injection of autowired dependencies failed", ex);
		}
		return pvs;
	}
```



比如 @Autowire

```java
private InjectionMetadata buildAutowiringMetadata(final Class<?> clazz) {
		if (!AnnotationUtils.isCandidateClass(clazz, this.autowiredAnnotationTypes)) {
			return InjectionMetadata.EMPTY;
		}

		List<InjectionMetadata.InjectedElement> elements = new ArrayList<>();
		Class<?> targetClass = clazz;

		do {
			final List<InjectionMetadata.InjectedElement> currElements = new ArrayList<>();

			ReflectionUtils.doWithLocalFields(targetClass, field -> {
				MergedAnnotation<?> ann = findAutowiredAnnotation(field);
				if (ann != null) {
					if (Modifier.isStatic(field.getModifiers())) {
						if (logger.isInfoEnabled()) {
							logger.info("Autowired annotation is not supported on static fields: " + field);
						}
						return;
					}
					boolean required = determineRequiredStatus(ann);
					currElements.add(new AutowiredFieldElement(field, required));
				}
			});

			ReflectionUtils.doWithLocalMethods(targetClass, method -> {
				Method bridgedMethod = BridgeMethodResolver.findBridgedMethod(method);
				if (!BridgeMethodResolver.isVisibilityBridgeMethodPair(method, bridgedMethod)) {
					return;
				}
				MergedAnnotation<?> ann = findAutowiredAnnotation(bridgedMethod);
				if (ann != null && method.equals(ClassUtils.getMostSpecificMethod(method, clazz))) {
					if (Modifier.isStatic(method.getModifiers())) {
						if (logger.isInfoEnabled()) {
							logger.info("Autowired annotation is not supported on static methods: " + method);
						}
						return;
					}
					if (method.getParameterCount() == 0) {
						if (logger.isInfoEnabled()) {
							logger.info("Autowired annotation should only be used on methods with parameters: " +
									method);
						}
					}
					boolean required = determineRequiredStatus(ann);
					PropertyDescriptor pd = BeanUtils.findPropertyForMethod(bridgedMethod, clazz);
					currElements.add(new AutowiredMethodElement(method, required, pd));
				}
			});

			elements.addAll(0, currElements);
			targetClass = targetClass.getSuperclass();
		}
		while (targetClass != null && targetClass != Object.class);

		return InjectionMetadata.forElements(elements, clazz);
	}

	@Nullable
	private MergedAnnotation<?> findAutowiredAnnotation(AccessibleObject ao) {
		MergedAnnotations annotations = MergedAnnotations.from(ao);
		for (Class<? extends Annotation> type : this.autowiredAnnotationTypes) {
			MergedAnnotation<?> annotation = annotations.get(type);
			if (annotation.isPresent()) {
				return annotation;
			}
		}
		return null;
	}
```





#### 核心注入：  field.set(target, getResourceToInject(target, requestingBeanName));

```
/**
 * Either this or {@link #getResourceToInject} needs to be overridden.
 */
protected void inject(Object target, @Nullable String requestingBeanName, @Nullable PropertyValues pvs)
      throws Throwable {

   if (this.isField) {
      Field field = (Field) this.member;
      ReflectionUtils.makeAccessible(field);
      field.set(target, getResourceToInject(target, requestingBeanName));
   }
   else {
      if (checkPropertySkipping(pvs)) {
         return;
      }
      try {
         Method method = (Method) this.member;
         ReflectionUtils.makeAccessible(method);
         method.invoke(target, getResourceToInject(target, requestingBeanName));
      }
      catch (InvocationTargetException ex) {
         throw ex.getTargetException();
      }
   }
}
```



#### org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory#initializeBean(java.lang.String, java.lang.Object, org.springframework.beans.factory.support.RootBeanDefinition)



```java
/**
	 * Initialize the given bean instance, applying factory callbacks
	 * as well as init methods and bean post processors.
	 * <p>Called from {@link #createBean} for traditionally defined beans,
	 * and from {@link #initializeBean} for existing bean instances.
	 * @param beanName the bean name in the factory (for debugging purposes)
	 * @param bean the new bean instance we may need to initialize
	 * @param mbd the bean definition that the bean was created with
	 * (can also be {@code null}, if given an existing bean instance)
	 * @return the initialized bean instance (potentially wrapped)
	 * @see BeanNameAware
	 * @see BeanClassLoaderAware
	 * @see BeanFactoryAware
	 * @see #applyBeanPostProcessorsBeforeInitialization
	 * @see #invokeInitMethods
	 * @see #applyBeanPostProcessorsAfterInitialization
	 */
	protected Object initializeBean(String beanName, Object bean, @Nullable RootBeanDefinition mbd) {
		if (System.getSecurityManager() != null) {
			AccessController.doPrivileged((PrivilegedAction<Object>) () -> {
				invokeAwareMethods(beanName, bean);
				return null;
			}, getAccessControlContext());
		}
		else {
			invokeAwareMethods(beanName, bean);
		}

		Object wrappedBean = bean;
		if (mbd == null || !mbd.isSynthetic()) {
			wrappedBean = applyBeanPostProcessorsBeforeInitialization(wrappedBean, beanName);
		}

		try {
			invokeInitMethods(beanName, wrappedBean, mbd);
		}
		catch (Throwable ex) {
			throw new BeanCreationException(
					(mbd != null ? mbd.getResourceDescription() : null),
					beanName, "Invocation of init method failed", ex);
		}
		if (mbd == null || !mbd.isSynthetic()) {
			wrappedBean = applyBeanPostProcessorsAfterInitialization(wrappedBean, beanName);
		}

		return wrappedBean;
	}
```



#### BeanPostProcessor执行

`org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory#applyBeanPostProcessorsBeforeInitialization`

```java
@Override
	public Object applyBeanPostProcessorsBeforeInitialization(Object existingBean, String beanName)
			throws BeansException {

		Object result = existingBean;
		for (BeanPostProcessor processor : getBeanPostProcessors()) {
			Object current = processor.postProcessBeforeInitialization(result, beanName);
			if (current == null) {
				return result;
			}
			result = current;
		}
		return result;
	}
```

![image-20220217172901259](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217172901259.png)





#### BeanPostProcessor 前后置处理器以及INIT方法



![image-20220217173044090](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220217173044090.png)





### finishRefresh

扩展分析  扩展点： initLifecycleProcessor

```java
/**
	 * Finish the refresh of this context, invoking the LifecycleProcessor's
	 * onRefresh() method and publishing the
	 * {@link org.springframework.context.event.ContextRefreshedEvent}.
	 */
	protected void finishRefresh() {
		// Clear context-level resource caches (such as ASM metadata from scanning).
		clearResourceCaches();

		// Initialize lifecycle processor for this context.
		initLifecycleProcessor();

		// Propagate refresh to lifecycle processor first.
		getLifecycleProcessor().onRefresh();

		// Publish the final event.
		publishEvent(new ContextRefreshedEvent(this));

		// Participate in LiveBeansView MBean, if active.
		LiveBeansView.registerApplicationContext(this);
	}
```

