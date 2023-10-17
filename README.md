# LearnOpenGL
LearnOpenGL文档学习

## 第1节 入门 <br>
  1. 你好，窗口 <br>
  2. 你好，三角形 <br>
    - Exercise_01 添加更多顶点到数据中，使用glDrawArrays，尝试绘制两个
        彼此相连的三角形 <br>
    - Exercise_02 创建相同的两个三角形，但对它们的数据使用不同的VAO和VBO <br>
    - Exercise_03 创建两个着色器程序，第二个程序使用一个不同的片段着色器，
        输出黄色；再次绘制这两个三角形，让其中一个输出为黄色 <br>
  3. 着色器GLSL <br>
    - Exercise_01 修改顶点着色器让三角形上下颠倒 <br>
    - Exercise_02 使用uniform定义一个水平偏移量，在顶点着色器中使用这个
        偏移量把三角形移动到屏幕右侧 <br>
    - Exercise_03 使用out关键字把顶点位置输出到片段着色器，并将片段的颜色设置为
        与顶点位置相等（来看看连顶点位置值都在三角形中被插值的结果）。做完这些后，
        尝试回答下面的问题：为什么在三角形的左下角是黑的? <br>
  4. 纹理Textures <br>
