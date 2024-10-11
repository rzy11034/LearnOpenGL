# LearnOpenGL
LearnOpenGL文档学习

## 第1章 入门 <br>
  1. 你好，窗口 Hello Window <br>
  2. 你好，三角形 Hello Triangle <br>
    - Exercise_01 添加更多顶点到数据中，使用glDrawArrays，尝试绘制两个
        彼此相连的三角形 <br>
    - Exercise_02 创建相同的两个三角形，但对它们的数据使用不同的VAO和VBO <br>
    - Exercise_03 创建两个着色器程序，第二个程序使用一个不同的片段着色器，
        输出黄色；再次绘制这两个三角形，让其中一个输出为黄色 <br>
  3. 着色器 GLSL <br>
    - Exercise_01 修改顶点着色器让三角形上下颠倒 <br>
    - Exercise_02 使用uniform定义一个水平偏移量，在顶点着色器中使用这个
        偏移量把三角形移动到屏幕右侧 <br>
    - Exercise_03 使用out关键字把顶点位置输出到片段着色器，并将片段的颜色设置为
        与顶点位置相等（来看看连顶点位置值都在三角形中被插值的结果）。做完这些后，
        尝试回答下面的问题：为什么在三角形的左下角是黑的? <br>
  4. 纹理 Textures <br>
    - Exercise_01 修改片段着色器，仅让笑脸图案朝另一个方向看 <br>
    - Exercise_02 尝试用不同的纹理环绕方式，设定一个从0.0f到2.0f范围内的（而不是
        原来的0.0f到1.0f）纹理坐标。试试看能不能在箱子的角落放置4个笑脸 <br>
    - Exercise_03 尝试在矩形上只显示纹理图像的中间一部分，修改纹理坐标，达到能看
        见单个的像素的效果。尝试使用GL_NEAREST的纹理过滤方式让像素显示得更清晰 <br>
    - Exercise_04 使用一个uniform变量作为mix函数的第三个参数来改变两个纹理可见度，
        使用上和下键来改变箱子或笑脸的可见度 <br>
  5. 变换 Transformations <br>
    - Exercise_01 使用应用在箱子上的最后一个变换，尝试将其改变为先旋转，后位移。
        看看发生了什么，试着想想为什么会发生这样的事情 <br>
    - Exercise_02 尝试再次调用glDrawElements画出第二个箱子，只使用变换将其摆放在
        不同的位置。让这个箱子被摆放在窗口的左上角，并且会不断的缩放（而不是旋转）。
        （sin函数在这里会很有用，不过注意使用sin函数时应用负值会导致物体被翻转）<br>
  6. 坐标系统 Coordinate Systems <br>
    - Exercise_01 对GLM的projection函数中的FoV和aspect-ratio参数进行实验。看能否
        搞懂它们是如何影响透视平截头体的。 <br>
    - Exercise_02 将观察矩阵在各个方向上进行位移，来看看场景是如何改变的。注意把
        观察矩阵当成摄像机对象。 <br>
    - Exercise_03 使用模型矩阵只让是3倍数的箱子旋转（以及第1个箱子），而让剩下的
        箱子保持静止。 <br>
  7. 摄像机	Camera <br>
    - Exercise_01 看看你是否能够修改摄像机类，使得其能够变成一个真正的FPS摄像机
        （也就是说不能够随意飞行）；你只能够呆在xz平面上。 <br>
    - Exercise_02 试着创建你自己的LookAt函数，其中你需要手动创建一个我们在一开始
        讨论的观察矩阵。用你的函数实现来替换GLM的LookAt函数，看看它是否还能一样
        地工作。 <br>

## 第2章 光照 <br>
  1. 颜色	Colors <br>
  2. 基础光照 Basic Lighting <br>
    - Exercise_01 目前，我们的光源是静止的，你可以尝试使用sin或cos函数让光源在
        场景中来回移动。观察光照随时间的改变能让你更容易理解风氏光照模型。<br>
    - Exercise_02 尝试使用不同的环境光、漫反射和镜面强度，观察它们怎么是影响光照
        效果的。同样，尝试实验一下镜面光照的反光度因子。尝试理解为什么某一个值能
        够有着特定视觉输出。<br>
    - Exercise_03 在观察空间（而不是世界空间）中计算风氏光照。<br>
    - Exercise_04 尝试实现一个Gouraud着色（而不是风氏着色）。如果你做对了话，立
        方体的光照应该会看起来有些奇怪，尝试推理为什么它会看起来这么奇怪。<br>
  3. 材质 Materials <br>
    - Exercise_01 你能做到这件事吗，改变光照颜色导致改变光源立方体的颜色。<br>
    - Exercise_02 你能像教程一开始那样，通过定义相应的材质来模拟现实世界的物体吗？
        注意材质表格中的环境光值与漫反射值不一样，它们没有考虑光照的强度。要想正
        确地设置它们的值，你需要将所有的光照强度都设置为vec3(1.0)，这样才能得到
        一致的输出：青色塑料(Cyan Plastic)容器。<br>
  4. 光照贴图 Lighting maps <br>
    - Exercise_01 调整光源的环境光、漫反射和镜面光向量，看看它们如何影响箱子的
        视觉输出。<br>
    - Exercise_02 尝试在片段着色器中反转镜面光贴图的颜色值，让木头显示镜面高光而
        钢制边缘不反光（由于钢制边缘中有一些裂缝，边缘仍会显示一些镜面高光，虽然
        强度会小很多）。<br>
    - Exercise 03 使用漫反射贴图创建一个彩色而不是黑白的镜面光贴图，看看结果看起
        来并不是那么真实了。<br>
    - Exercise 04 添加一个叫做放射光贴图(Emission Map)的东西，它是一个储存了每个
        片段的发光值(Emission Value)的贴图。发光值是一个包含（假设）光源的物体发
        光(Emit)时可能显现的颜色，这样的话物体就能够忽略光照条件进行发光(Glow)。
        游戏中某个物体在发光的时候，你通常看到的就是放射光贴图（比如 机器人的眼，
        或是箱子上的灯带）。将这个纹理（作者为 creativesam）作为放射光贴图添加到
        箱子上，产生这些字母都在发光的效果。<br>
  5. 投光物	Light casters <br>
  6. 多光源 Multiple lights <br>
    - Exercise_01 通过调节光照属性变量，重现最后一张图片上不同的氛围 <br>

## 第3章 模型加载 <br>
  1. 模型加载	Model Loading <br>

## 第4章 高级OpenGL <br>
  1. 深度测试 Depth testing <br>
  2. 模板测试 Stencil testing <br>
  3. 混合 Blending  <br>
  4. 面剔除	Face culling <br>
    - Exercise_01 你能够重新定义顶点数据，将每个三角形设置为顺时针顺序，并将顺时
        针的三角形设置为正向面，仍将场景渲染出来吗? <br>
  5. 帧缓冲 Framebuffers <br>
    - Exercise_01 你能使用framebuffers创建一个后视镜吗?为此，你必须绘制两次场景:
        一次将相机旋转180度，另一次将相机正常旋转。试着在屏幕顶部创建一个小的四
        边形来应用镜像纹理 <br>
  6. 立方体贴图 Cubemaps <br>
  7. 高级数据 Advanced Data <br>
  8. 高级GLSL Advanced GLSL <br>
  9. 几何着色器 Geometry Shader <br>
  10. 实例化 Instancing <br>
  11. 抗锯齿 Anti Aliasing <br>

## 第5章 高级光照 <br>
  1. 高级光照	Advanced Lighting <br>
  2. Gamma校正 Gamma Correction <br>
  3. 阴影 Shadow <br>
    - 1) 阴影映射 Shadow Mapping <br>
    - 2) 点光源阴影	Point Shadows <br>
  4. 法线贴图	Normal Mapping <br>
  5. 视差贴图 Parallax Mapping <br>
  6. 高动态范围 HDR <br>
  7. 泛光	Bloom <br>
  8. 延迟着色法	Deferred Shading <br>
  9. 屏幕空间环境光遮蔽 SSAO <br>

## 第6章 基于物理的渲染 PBR <br>
  1. 光照	Lighting <br>
  2. 基于图像的光照 IBL(Image based lighting) <br>
    - 1) 漫反射辐照度	Diffuse irradiance <br>
    - 2) 镜面反射 IBL <br>

## 第7章 实战 <br>
  1. 调试	Debugging <br>
