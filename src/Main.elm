module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL
import WebGL.Texture as Tex



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Float
    , resolution : ( Int, Int )
    , mouse :
        { pos : ( Float, Float )
        , pressed : Bool
        }
    }


type alias Flags =
    { width : Int, height : Int }


init : Flags -> ( Model, Cmd Msg )
init { width, height } =
    ( Model 0 ( width, height ) { pos = ( 0, 0 ), pressed = False }, Cmd.none )


type Msg
    = TimeDelta Float
    | Moved Float Float
    | Up
    | Down


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ time, mouse } as model) =
    let
        _ =
            Debug.log "Model" model
    in
    case msg of
        TimeDelta delta ->
            ( { model | time = time + delta * 0.1 }, Cmd.none )

        Moved x y ->
            ( { model | mouse = { mouse | pos = ( x, y ) } }, Cmd.none )

        Down ->
            ( { model | mouse = { mouse | pressed = True } }, Cmd.none )

        Up ->
            ( { model | mouse = { mouse | pressed = False } }, Cmd.none )



-- SUBSCRIPTIONS


mouseDecoder =
    Decode.map2 Moved
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ E.onAnimationFrameDelta TimeDelta
        , E.onMouseDown (Decode.succeed Down)
        , E.onMouseUp (Decode.succeed Up)
        , E.onMouseMove mouseDecoder
        ]



-- VIEW

type alias Uniforms =
    { time : Float
    , size : Vec2
    , mouseXY : Vec2
    , pressed : Int
    }

view : Model -> Html msg
view { time, resolution, mouse } =
    let
        { pos, pressed } =
            mouse
    in
    let
        ( w, h ) =
            resolution
    in
    WebGL.toHtml
        [ width w, height h, style "display" "block" ]
        [ WebGL.entity vert frag mesh (Uniforms (time * 0.005) (vec2 (toFloat w) (toFloat h)) (vec2 (Tuple.first pos) (Tuple.second pos)) (if pressed then 1 else 0)) ]


type alias Vertex =
    { position : Vec3 }


mesh : WebGL.Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0.99999)
          , Vertex (vec3 -1 3 0.99999)
          , Vertex (vec3 3 -1 0.99999)
          )
        ]


vert : WebGL.Shader Vertex Uniforms {}
vert =
    [glsl| 
        attribute vec3 position;
        
        void main () {
            gl_Position = vec4(position, 1.);
        }
    |]


frag : WebGL.Shader {} Uniforms {}
frag =
    [glsl|
        precision highp float;
        
        uniform float time;
        uniform vec2 size;
        uniform vec2 mouseXY;
        uniform int pressed;

        float circle(vec2 uv, vec2 pos, float radius) {
            return length(pos - uv) - radius; 
        }

        void main() {
            vec2 pos = gl_FragCoord.xy / size;
            float t1 = sin(time) * 0.5 + 0.5;
            float t2 = sin(time * 2.) * 0.5 + 0.5;
            float t3 = sin(time * 3.) * 0.5 + 0.5;

            vec3 col = vec3(t1, t2, t3);

            vec2 mouse = mouseXY / size;
            mouse.y = 1. - mouse.y;

            if (pressed > 0) {
                float dist = circle(pos, mouse, 1e-32);
                col = mix( col,1.-col, step( dist, 0.05));
            }
            gl_FragColor = vec4(col, 1.);
        } 
    |]
