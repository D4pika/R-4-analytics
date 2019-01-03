library(tidyverse)
mtcars<-dput(mtcars)

mpg

ggplot(data =mpg)+geom_point(mapping=aes(x=displ, y=hwy))
nrow(mpg)
ncol(mpg)
plot(mpg$hwy,mpg$cyl)
plot(mpg$class,mpg$drv)


ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,color=class))
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,shape=class))
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,size=cyl,alpha=class))

ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy),color="blue")
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy,alpha=cty,color=cty))
                            
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,stroke=2))                            

ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,color=displ<5))
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,stroke=displ>4))  

#common problem with ggplot()

ggplot(data = mpg) 
+ geom_point(mapping = aes(x = displ, y = hwy))


g<-ggplot(data=mpg,mapping=aes(x=displ,y=hwy),xlab="displacement",ylab="Miles driven on Highway")

g+facet_wrap(.~cyl)+geom_point(aes(color=class))

g+facet_grid(class~cyl)+geom_point(aes())
g+facet_grid(drv~cyl)+geom_point(aes(color=class))
g+facet_grid(.~cyl)+geom_point()

g+facet_wrap(.~cty)+geom_point()
g+facet_grid(cyl~.)+geom_point()
g+facet_grid(cyl~class)+geom_point(aes())


#introducing geom_smooth()

g+ geom_smooth()
g+geom_smooth(mapping=aes(linetype=factor(cyl)))
g+geom_smooth(aes(linetype =drv))
g+geom_smooth(aes(group=drv))
g+geom_smooth(aes(color=class)) + geom_point()

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
)
g+stat_summary()
g+geom_bar()
g+stat_smooth()
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..),group=1)
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..,group=1))+coord_polar()
ggplot(mpg, aes(cyl, hwy)) + geom_jitter(width = 0.25)
ggplot(mpg, aes(cyl, hwy)) + geom_count(width = 0.25)
ggplot(mpg, aes(cyl, hwy,color=class)) + geom_boxplot(width = 0.5)


nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
us <- map_data("ny")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")+coord_map()


ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +coord_fixed(ratio=1)
  