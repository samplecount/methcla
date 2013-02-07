//
//  Shader.fsh
//  Methcla
//
//  Created by Stefan Kersten on 2/6/13.
//
//

varying lowp vec4 colorVarying;

void main()
{
    gl_FragColor = colorVarying;
}
