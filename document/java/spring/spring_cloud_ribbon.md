

## Ribbon启动类

`org.springframework.cloud.netflix.ribbon.RibbonAutoConfiguration`

### RibbonAutoConfiguration

![image-20220226191429974](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220226191429974.png)





```java
@Configuration
@Conditional(RibbonAutoConfiguration.RibbonClassesConditions.class)
@RibbonClients
@AutoConfigureAfter(
		name = "org.springframework.cloud.netflix.eureka.EurekaClientAutoConfiguration")
@AutoConfigureBefore({ LoadBalancerAutoConfiguration.class,
		AsyncLoadBalancerAutoConfiguration.class })
@EnableConfigurationProperties({ RibbonEagerLoadProperties.class,
		ServerIntrospectorProperties.class })
public class RibbonAutoConfiguration {
```

所有condition都被加载才加载！ 这个注意下： 

#### AllNestedConditions

这个类什么意思，大概意思是继承这个 类下的所有条件都满足，才能通过

`org.springframework.boot.autoconfigure.condition.AllNestedConditions`



#### RibbonClients

```java
/**
 * Convenience annotation that allows user to combine multiple <code>@RibbonClient</code>
 * annotations on a single class (including in Java 7).
 *
 * @author Dave Syer
 */
@Configuration(proxyBeanMethods = false)
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Documented
@Import(RibbonClientConfigurationRegistrar.class)
public @interface RibbonClients {

	RibbonClient[] value() default {};

	Class<?>[] defaultConfiguration() default {};

}

```



#### RibbonClientConfigurationRegistrar

`org.springframework.cloud.netflix.ribbon.RibbonClientConfigurationRegistrar`

这个类和feign对应的类类似，都是加载注册对应的`BeanDefinition` 中，方便在类初始化时候进行初始化



org.springframework.cloud.netflix.ribbon.RibbonClientConfigurationRegistrar



ribbon和nacos的关联点： 

![image-20220226211655819](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220226211655819.png)



ribbon和服务注册中心 eureka或者nacos关联点： @RibbonClients

```java
/**
 * Convenience annotation that allows user to combine multiple <code>@RibbonClient</code>
 * annotations on a single class (including in Java 7).
 *
 * @author Dave Syer
 */
@Configuration(proxyBeanMethods = false)
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Documented
@Import(RibbonClientConfigurationRegistrar.class)
public @interface RibbonClients {

	RibbonClient[] value() default {};

	Class<?>[] defaultConfiguration() default {};

}
```





![image-20220227095840067](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220227095840067.png)



#### RibbonAutoConfiguration

```java
/**
 * Auto configuration for Ribbon (client side load balancing).
 *
 * @author Spencer Gibb
 * @author Dave Syer
 * @author Biju Kunjummen
 */
@Configuration
@Conditional(RibbonAutoConfiguration.RibbonClassesConditions.class)
@RibbonClients
@AutoConfigureAfter(
		name = "org.springframework.cloud.netflix.eureka.EurekaClientAutoConfiguration")
@AutoConfigureBefore({ LoadBalancerAutoConfiguration.class,
		AsyncLoadBalancerAutoConfiguration.class })
@EnableConfigurationProperties({ RibbonEagerLoadProperties.class,
		ServerIntrospectorProperties.class })
public class RibbonAutoConfiguration {

```

这个类上标注了 `@RibbonClients`注解， 看这个注解的定义： 

```java
/**
 * Convenience annotation that allows user to combine multiple <code>@RibbonClient</code>
 * annotations on a single class (including in Java 7).
 *
 * @author Dave Syer
 */
@Configuration(proxyBeanMethods = false)
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Documented
@Import(RibbonClientConfigurationRegistrar.class)
public @interface RibbonClients {

	RibbonClient[] value() default {};

	Class<?>[] defaultConfiguration() default {};

}

```

#### @Import(RibbonClientConfigurationRegistrar.class)

`org.springframework.cloud.netflix.ribbon.RibbonClientConfigurationRegistrar`



这段代码和feign的加载类似，都是通过扫描这个注入 @RibbonClients  标注的类，并生成对应的`BeanDefinition`

```java
**
 * @author Dave Syer
 */
public class RibbonClientConfigurationRegistrar implements ImportBeanDefinitionRegistrar {

	@Override
	public void registerBeanDefinitions(AnnotationMetadata metadata,
			BeanDefinitionRegistry registry) {
		Map<String, Object> attrs = metadata
				.getAnnotationAttributes(RibbonClients.class.getName(), true);
		if (attrs != null && attrs.containsKey("value")) {
			AnnotationAttributes[] clients = (AnnotationAttributes[]) attrs.get("value");
			for (AnnotationAttributes client : clients) {
				registerClientConfiguration(registry, getClientName(client),
						client.get("configuration"));
			}
		}
		if (attrs != null && attrs.containsKey("defaultConfiguration")) {
			String name;
			if (metadata.hasEnclosingClass()) {
				name = "default." + metadata.getEnclosingClassName();
			}
			else {
				name = "default." + metadata.getClassName();
			}
			registerClientConfiguration(registry, name,
					attrs.get("defaultConfiguration"));
		}
		Map<String, Object> client = metadata
				.getAnnotationAttributes(RibbonClient.class.getName(), true);
		String name = getClientName(client);
		if (name != null) {
			registerClientConfiguration(registry, name, client.get("configuration"));
		}
	}

	private String getClientName(Map<String, Object> client) {
		if (client == null) {
			return null;
		}
		String value = (String) client.get("value");
		if (!StringUtils.hasText(value)) {
			value = (String) client.get("name");
		}
		if (StringUtils.hasText(value)) {
			return value;
		}
		throw new IllegalStateException(
				"Either 'name' or 'value' must be provided in @RibbonClient");
	}

	private void registerClientConfiguration(BeanDefinitionRegistry registry, Object name,
			Object configuration) {
		BeanDefinitionBuilder builder = BeanDefinitionBuilder
				.genericBeanDefinition(RibbonClientSpecification.class);
		builder.addConstructorArgValue(name);
		builder.addConstructorArgValue(configuration);
		registry.registerBeanDefinition(name + ".RibbonClientSpecification",
				builder.getBeanDefinition());
	}

}

```



注册生成对应的beanDefinition

feign以及ribbon生成的`BeanDefinition`如下： 

```java
//ribbion beanDefinition 部分信息
default.com.alibaba.cloud.nacos.ribbon.RibbonNacosAutoConfiguration.RibbonClientSpecification -> {GenericBeanDefinition@5692} "Generic bean: class [org.springframework.cloud.netflix.ribbon.RibbonClientSpecification]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
  
 //feign BeanDefinition部分信息： 
  default.com.xs.micro.consumer.MicroConsumerApplication.FeignClientSpecification -> {GenericBeanDefinition@5588} "Generic bean: class [org.springframework.cloud.openfeign.FeignClientSpecification]; scope=; abstract=false; lazyInit=null; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null"
  
```

![image-20220301102416408](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220301102416408.png)



#### org.springframework.cloud.netflix.ribbon.RibbonAutoConfiguration#springClientFactory



```java
@Bean
	@ConditionalOnMissingBean
	public SpringClientFactory springClientFactory() {
    //基础类 ，很重要，后期feign和ribbon都会用到这个类
		SpringClientFactory factory = new SpringClientFactory();
		factory.setConfigurations(this.configurations);
		return factory;
	}
```

#### org.springframework.cloud.netflix.ribbon.SpringClientFactory#SpringClientFactory

```java
public SpringClientFactory() {
		super(RibbonClientConfiguration.class, NAMESPACE, "ribbon.client.name");
	}

```



#### org.springframework.cloud.context.named.NamedContextFactory

```java
public abstract class NamedContextFactory<C extends NamedContextFactory.Specification>
		implements DisposableBean, ApplicationContextAware {

	private final String propertySourceName;

	private final String propertyName;

  //这里存储父子容器的新的子容器
	private Map<String, AnnotationConfigApplicationContext> contexts = new ConcurrentHashMap<>();

  //存储相关的配置信息
	private Map<String, C> configurations = new ConcurrentHashMap<>();

  //这里是启动父容器，如果是web，之前分析过的spring-boot的时候，就是spring boot的容器,父容器就是这个： 
//  org.springframework.boot.web.servlet.context.AnnotationConfigServletWebApplicationContext
	private ApplicationContext parent;

	private Class<?> defaultConfigType;

  //初始化构造方法
	public NamedContextFactory(Class<?> defaultConfigType, String propertySourceName,
			String propertyName) {
		this.defaultConfigType = defaultConfigType;
		this.propertySourceName = propertySourceName;
		this.propertyName = propertyName;
	}

```



#### org.springframework.cloud.netflix.ribbon.RibbonAutoConfiguration#loadBalancerClient

```java
@Bean
	@ConditionalOnMissingBean(LoadBalancerClient.class)
	public LoadBalancerClient loadBalancerClient() {
    //上面创建的springClientFactory直接被注入了RibbonLoadBalancerClient
		return new RibbonLoadBalancerClient(springClientFactory());
	}

```



#### org.springframework.cloud.netflix.ribbon.RibbonLoadBalancerClient

这个类是负载均衡的核心类，用于选择不同的server进行处理，核心类： `org.springframework.cloud.netflix.ribbon.RibbonLoadBalancerClient#choose(java.lang.String, java.lang.Object)`

```java
/**
	 * New: Select a server using a 'key'.
	 * @param serviceId of the service to choose an instance for
	 * @param hint to specify the service instance
	 * @return the selected {@link ServiceInstance}
	 */
	public ServiceInstance choose(String serviceId, Object hint) {
    //从负载均衡的服务中选出一个服务实例信息
		Server server = getServer(getLoadBalancer(serviceId), hint);
		if (server == null) {
			return null;
		}
		return new RibbonServer(serviceId, server, isSecure(server, serviceId),
				serverIntrospector(serviceId).getMetadata(server));
	}
```



#### org.springframework.cloud.client.loadbalancer.LoadBalancerClient

对应的 接口有详细的描述信息： 

```java
/**
 * Represents a client-side load balancer.
 *
 * @author Spencer Gibb
 */
public interface LoadBalancerClient extends ServiceInstanceChooser {

	/**
	 * Executes request using a ServiceInstance from the LoadBalancer for the specified
	 * service.
	 * @param serviceId The service ID to look up the LoadBalancer.
	 * @param request Allows implementations to execute pre and post actions, such as
	 * incrementing metrics.
	 * @param <T> type of the response
	 * @throws IOException in case of IO issues.
	 * @return The result of the LoadBalancerRequest callback on the selected
	 * ServiceInstance.
	 */
	<T> T execute(String serviceId, LoadBalancerRequest<T> request) throws IOException;

	/**
	 * Executes request using a ServiceInstance from the LoadBalancer for the specified
	 * service.
	 * @param serviceId The service ID to look up the LoadBalancer.
	 * @param serviceInstance The service to execute the request to.
	 * @param request Allows implementations to execute pre and post actions, such as
	 * incrementing metrics.
	 * @param <T> type of the response
	 * @throws IOException in case of IO issues.
	 * @return The result of the LoadBalancerRequest callback on the selected
	 * ServiceInstance.
	 */
	<T> T execute(String serviceId, ServiceInstance serviceInstance,
			LoadBalancerRequest<T> request) throws IOException;

	/**
	 * Creates a proper URI with a real host and port for systems to utilize. Some systems
	 * use a URI with the logical service name as the host, such as
	 * http://myservice/path/to/service. This will replace the service name with the
	 * host:port from the ServiceInstance.
	 * @param instance service instance to reconstruct the URI
	 * @param original A URI with the host as a logical service name.
	 * @return A reconstructed URI.
	 */
	URI reconstructURI(ServiceInstance instance, URI original);

}

```



#### org.springframework.cloud.client.loadbalancer.ServiceInstanceChooser#choose

choose，负责通过servierId，找到对应的服务实例，

```java
/**
	 * Chooses a ServiceInstance from the LoadBalancer for the specified service.
	 * @param serviceId The service ID to look up the LoadBalancer.
	 * @return A ServiceInstance that matches the serviceId.
	 */
	ServiceInstance choose(String serviceId);

```



#### org.springframework.cloud.netflix.ribbon.RibbonLoadBalancerClient#choose(java.lang.String)

实现类信息： 

```java
@Override
	public ServiceInstance choose(String serviceId) {
		return choose(serviceId, null);
	}

/**
	 * New: Select a server using a 'key'.
	 * @param serviceId of the service to choose an instance for
	 * @param hint to specify the service instance
	 * @return the selected {@link ServiceInstance}
	 */
	public ServiceInstance choose(String serviceId, Object hint) {
    //从服务列表中获取Server
		Server server = getServer(getLoadBalancer(serviceId), hint);
		if (server == null) {
			return null;
		}
    //包装一下，返回Service
		return new RibbonServer(serviceId, server, isSecure(server, serviceId),
				serverIntrospector(serviceId).getMetadata(server));
	}
```



#### org.springframework.cloud.netflix.ribbon.RibbonLoadBalancerClient#getServer(com.netflix.loadbalancer.ILoadBalancer, java.lang.Object)



```java
protected Server getServer(ILoadBalancer loadBalancer, Object hint) {
		if (loadBalancer == null) {
			return null;
		}
		// Use 'default' on a null hint, or just pass it on?
		return loadBalancer.chooseServer(hint != null ? hint : "default");
	}
```



#### org.springframework.cloud.netflix.ribbon.RibbonLoadBalancerClient#getLoadBalancer

```java
//获取对应的负载均衡器
protected ILoadBalancer getLoadBalancer(String serviceId) {
		return this.clientFactory.getLoadBalancer(serviceId);
	}
```



#### org.springframework.cloud.netflix.ribbon.SpringClientFactory#getLoadBalancer

```java
/**
	 * Get the load balancer associated with the name.
	 * @param name name to search by
	 * @return {@link ILoadBalancer} instance
	 * @throws RuntimeException if any error occurs
	 */
	public ILoadBalancer getLoadBalancer(String name) {
    //获取对应的负载均衡器
		return getInstance(name, ILoadBalancer.class);
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
    //核心是调用下面的方法
		IClientConfig config = getInstance(name, IClientConfig.class);
		return instantiateWithConfig(getContext(name), type, config);
	}
```



#### org.springframework.cloud.context.named.NamedContextFactory#getInstance(java.lang.String, java.lang.Class<T>)

```java
public <T> T getInstance(String name, Class<T> type) {
    //获取name对应的上下文，如果没有就创建
		AnnotationConfigApplicationContext context = getContext(name);
		if (BeanFactoryUtils.beanNamesForTypeIncludingAncestors(context,
				type).length > 0) {
			return context.getBean(type);
		}
		return null;
	}
```



以上解析的调用栈，是在ribbion初始化参数`ribbon.eager-load.enabled`设置为false，进行延迟初始化，就是第一次调用时初始化处理的方式，什么时候真正初始化ribbon的这些配置上下文呢？？？第一笔调用时，进行初始化



### 以下继续分析RibbonAutoConfiguration中的方法

#### org.springframework.cloud.netflix.ribbon.RibbonAutoConfiguration#propertiesFactory

```java
	@Bean
	@ConditionalOnMissingBean
	public PropertiesFactory propertiesFactory() {
		return new PropertiesFactory();
	}

```

为了定义一个： [微服务名称].ribbon.[类名对应的特定值]



![image-20220301151720174](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220301151720174.png)



就是如下debug的这种格式： 

![image-20220301152548029](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220301152548029.png)



#### org.springframework.cloud.netflix.ribbon.RibbonAutoConfiguration#ribbonApplicationContextInitializer



```java
@Bean
	@ConditionalOnProperty("ribbon.eager-load.enabled")
	public RibbonApplicationContextInitializer ribbonApplicationContextInitializer() {
		return new RibbonApplicationContextInitializer(springClientFactory(),
				ribbonEagerLoadProperties.getClients());
	}
```



#### org.springframework.cloud.netflix.ribbon.RibbonEagerLoadProperties

```java
@ConfigurationProperties(prefix = "ribbon.eager-load")
public class RibbonEagerLoadProperties {
  //默认是false，不提前加载，等到第一次调用的时候才进行ribbon上下文的初始化工作    
	private boolean enabled = false;

	private List<String> clients;

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public List<String> getClients() {
		return clients;
	}

	public void setClients(List<String> clients) {
		this.clients = clients;
	}

}
```



如果修改：就可以初始化时候进行加载，另外为什么延迟加载，是否和注册中心eureka或者nacos没有加载更新注册中心服务列表有关系？？？ 

```java
ribbon.eager-load.enabled=true
ribbon.eager-load.clients=nacos-client
```



#### org.springframework.cloud.netflix.ribbon.RibbonApplicationContextInitializer

立即初始化的原理是，通过监听`ApplicationReadyEvent`事件，如果监听到，就进行响应的初始化

```java
protected void initialize() {
		if (clientNames != null) {
			for (String clientName : clientNames) {
        //初始化ribbon client对应的子容器上下文
				this.springClientFactory.getContext(clientName);
			}
		}
	}

	@Override
	public void onApplicationEvent(ApplicationReadyEvent event) {
		initialize();
	}

```



#### org.springframework.cloud.netflix.ribbon.RibbonAutoConfiguration.RibbonClientHttpRequestFactoryConfiguration#restTemplateCustomizer

```java
@Bean
		public RestTemplateCustomizer restTemplateCustomizer(
				final RibbonClientHttpRequestFactory ribbonClientHttpRequestFactory) {
			return restTemplate -> restTemplate
					.setRequestFactory(ribbonClientHttpRequestFactory);
		}
```



#### org.springframework.http.client.support.HttpAccessor#setRequestFactory

```java
org.springframework.http.client.support.HttpAccessor
  	/** Logger available to subclasses. */
	protected final Log logger = HttpLogging.forLogName(getClass());

	private ClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();

	private final List<ClientHttpRequestInitializer> clientHttpRequestInitializers = new ArrayList<>();


```

#### org.springframework.cloud.netflix.ribbon.RibbonClientHttpRequestFactory#createRequest

```java
@Override
	@SuppressWarnings("deprecation")
	public ClientHttpRequest createRequest(URI originalUri, HttpMethod httpMethod)
			throws IOException {
		String serviceId = originalUri.getHost();
		if (serviceId == null) {
			throw new IOException(
					"Invalid hostname in the URI [" + originalUri.toASCIIString() + "]");
		}
		IClientConfig clientConfig = this.clientFactory.getClientConfig(serviceId);
		RestClient client = this.clientFactory.getClient(serviceId, RestClient.class);
		HttpRequest.Verb verb = HttpRequest.Verb.valueOf(httpMethod.name());

		return new RibbonHttpRequest(originalUri, verb, client, clientConfig);
	}

```



#### 注意： 

RestTemplate 本质上execute，执行的是 HttpUrlConnection

```java
public interface RestTemplateCustomizer {

	void customize(RestTemplate restTemplate);

}
```

```

```



#### org.springframework.http.client.support.HttpAccessor#createRequest

最终走的是

```java
private ClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
```





#### org.springframework.http.client.SimpleClientHttpRequestFactory

这里的请求连接，默认走的是`HttpURLConnection`这个链接，在最后调用的时候能看到

```java
/**
	 * Opens and returns a connection to the given URL.
	 * <p>The default implementation uses the given {@linkplain #setProxy(java.net.Proxy) proxy} -
	 * if any - to open a connection.
	 * @param url the URL to open a connection to
	 * @param proxy the proxy to use, may be {@code null}
	 * @return the opened connection
	 * @throws IOException in case of I/O errors
	 */
	protected HttpURLConnection openConnection(URL url, @Nullable Proxy proxy) throws IOException {
		URLConnection urlConnection = (proxy != null ? url.openConnection(proxy) : url.openConnection());
		if (!HttpURLConnection.class.isInstance(urlConnection)) {
			throw new IllegalStateException("HttpURLConnection required for [" + url + "] but got: " + urlConnection);
		}
		return (HttpURLConnection) urlConnection;
	}

```







#### 关于ribbion负载均衡重试的说明

默认只有http get进行重试，如果所有的都需要重试，需要重新设置

`org.springframework.cloud.openfeign.ribbon.FeignLoadBalancer#getRequestSpecificRetryHandler`

```java
@Override
	public RequestSpecificRetryHandler getRequestSpecificRetryHandler(
			RibbonRequest request, IClientConfig requestConfig) {
		if (this.ribbon.isOkToRetryOnAllOperations()) {
			return new RequestSpecificRetryHandler(true, true, this.getRetryHandler(),
					requestConfig);
		}
		if (!request.toRequest().httpMethod().name().equals("GET")) {
			return new RequestSpecificRetryHandler(true, false, this.getRetryHandler(),
					requestConfig);
		}
		else {
			return new RequestSpecificRetryHandler(true, true, this.getRetryHandler(),
					requestConfig);
		}
	}
```



![image-20220301154142922](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220301154142922.png)



### 负载均衡中的ping探测服务状态



#### com.netflix.loadbalancer.LoadBalancerBuilder

```java
    private IPing ping = new DummyPing();


```

默认是dummyPing()，直接返回true

```java
public class DummyPing extends AbstractLoadBalancerPing {

    public DummyPing() {
    }

    public boolean isAlive(Server server) {
      //直接默认连接均可用的
        return true;
    }

    @Override
    public void initWithNiwsConfig(IClientConfig clientConfig) {
    }
}
```



也可以进行探测，探测的意义，就是检查服务列表中的InstanceInfo的状态，如果是up就是可以提供服务

#### com.netflix.niws.loadbalancer.NIWSDiscoveryPing#isAlive

```java
 public boolean isAlive(Server server) {
        boolean isAlive = true;
        if (server != null && server instanceof DiscoveryEnabledServer) {
            DiscoveryEnabledServer dServer = (DiscoveryEnabledServer)server;
            InstanceInfo instanceInfo = dServer.getInstanceInfo();
            if (instanceInfo != null) {
                InstanceStatus status = instanceInfo.getStatus();
                if (status != null) {
                    isAlive = status.equals(InstanceStatus.UP);
                }
            }
        }

        return isAlive;
    }
```





#### nacos ribbon关联点核心重要

#### com.alibaba.cloud.nacos.ribbon.NacosRibbonClientConfiguration

```java
/*
 * Copyright 2013-2018 the original author or authors.
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

package com.alibaba.cloud.nacos.ribbon;

import com.alibaba.cloud.nacos.NacosDiscoveryProperties;
import com.netflix.client.config.IClientConfig;
import com.netflix.loadbalancer.ServerList;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.cloud.netflix.ribbon.PropertiesFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * integrated Ribbon by default.
 *
 * @author xiaojing
 * @author liujunjie
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnRibbonNacos
public class NacosRibbonClientConfiguration {

	@Autowired
	private PropertiesFactory propertiesFactory;

	@Bean
	@ConditionalOnMissingBean
	public ServerList<?> ribbonServerList(IClientConfig config,
			NacosDiscoveryProperties nacosDiscoveryProperties) {
		if (this.propertiesFactory.isSet(ServerList.class, config.getClientName())) {
			ServerList serverList = this.propertiesFactory.get(ServerList.class, config,
					config.getClientName());
			return serverList;
		}
    //服务列表
		NacosServerList serverList = new NacosServerList(nacosDiscoveryProperties);
		serverList.initWithNiwsConfig(config);
		return serverList;
	}

	@Bean
	@ConditionalOnMissingBean
	public NacosServerIntrospector nacosServerIntrospector() {
		return new NacosServerIntrospector();
	}

}

```





#### com.netflix.loadbalancer.ServerList

Ribbon和nacos的关联点:

![image-20220301161601009](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220301161601009.png)





#### com.alibaba.cloud.nacos.ribbon.NacosRibbonClientConfiguration#nacosServerIntrospector



获取nacosServer的元数据信息：

```java
/*
 * Copyright 2013-2018 the original author or authors.
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

package com.alibaba.cloud.nacos.ribbon;

import java.util.Map;

import com.netflix.loadbalancer.Server;

import org.springframework.cloud.netflix.ribbon.DefaultServerIntrospector;

/**
 * @author xiaojing
 */
public class NacosServerIntrospector extends DefaultServerIntrospector {

	@Override
	public Map<String, String> getMetadata(Server server) {
		if (server instanceof NacosServer) {
			return ((NacosServer) server).getMetadata();
		}
		return super.getMetadata(server);
	}

	@Override
	public boolean isSecure(Server server) {
		if (server instanceof NacosServer) {
			return Boolean.valueOf(((NacosServer) server).getMetadata().get("secure"));
		}

		return super.isSecure(server);
	}

}

```



继续研究： 

#### org.springframework.cloud.client.loadbalancer.LoadBalancerAutoConfiguration



#### org.springframework.cloud.client.loadbalancer.LoadBalancerAutoConfiguration





#### com.netflix.loadbalancer.ZoneAwareLoadBalancer#chooseServer

```java
  @Override
    public Server chooseServer(Object key) {
        if (!ENABLED.get() || getLoadBalancerStats().getAvailableZones().size() <= 1) {
            logger.debug("Zone aware logic disabled or there is only one zone");
            return super.chooseServer(key);
        }
```



#### com.netflix.loadbalancer.BaseLoadBalancer#chooseServer

```java
 public Server chooseServer(Object key) {
        if (counter == null) {
            counter = createCounter();
        }
        counter.increment();
        if (rule == null) {
            return null;
        } else {
            try {
                return rule.choose(key);
            } catch (Exception e) {
                logger.warn("LoadBalancer [{}]:  Error choosing server for key {}", name, key, e);
                return null;
            }
        }
    }
```



#### com.netflix.loadbalancer.PredicateBasedRule#choose

```java

```



#### com.netflix.loadbalancer.AbstractServerPredicate#chooseRoundRobinAfterFiltering(java.util.List<com.netflix.loadbalancer.Server>, java.lang.Object)

```java
/**
     * Choose a server in a round robin fashion after the predicate filters a given list of servers and load balancer key. 
     */
    public Optional<Server> chooseRoundRobinAfterFiltering(List<Server> servers, Object loadBalancerKey) {
        List<Server> eligible = getEligibleServers(servers, loadBalancerKey);
        if (eligible.size() == 0) {
            return Optional.absent();
        }
        return Optional.of(eligible.get(incrementAndGetModulo(eligible.size())));
    }
        
```





#### com.netflix.loadbalancer.AbstractServerPredicate#incrementAndGetModulo

负载均衡算法求出一个可用的服务： 

```java
/**
     * Referenced from RoundRobinRule
     * Inspired by the implementation of {@link AtomicInteger#incrementAndGet()}.
     *
     * @param modulo The modulo to bound the value of the counter.
     * @return The next value.
     */
    private int incrementAndGetModulo(int modulo) {
        for (;;) {
            int current = nextIndex.get();
            int next = (current + 1) % modulo;
            if (nextIndex.compareAndSet(current, next) && current < modulo)
                return current;
        }
    }
```

![image-20220301172017493](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220301172017493.png)