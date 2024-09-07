#version 330 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 texCoord;
uniform mat4 u_transform;
out vec2 TexCoord;

void main()
{
    gl_Position = u_transform * vec4(aPos, 0, 1.0);
    TexCoord = texCoord;
}