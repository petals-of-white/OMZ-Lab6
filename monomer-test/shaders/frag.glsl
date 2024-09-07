#version 330 core

out vec4 FragColor;
in vec2 TexCoord;
uniform sampler2D tex2d_sampler;

void main()
{
    FragColor = vec4(texture(tex2d_sampler, TexCoord).rrr, 1);
}