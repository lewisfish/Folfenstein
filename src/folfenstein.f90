module folfenstein

    use, intrinsic :: iso_c_binding, only : c_int64_t, c_ptr, c_int, c_int32_t
    
    use sdl2
    
    use video, only : state_t, buffer_t, black, red, green, blue, pink,grey,texture_t
    use vector_class, only : vector

    implicit none

    type :: hit_t
        integer :: val, side
        type(vector) :: pos
    end type hit_t

    interface hit_t
        module procedure init_hit
    end interface hit_t

    type(buffer_t) :: buffer

    type(state_t) :: state

    integer(kind=c_int), parameter :: SCREEN_WIDTH  = 384
    integer(kind=c_int), parameter :: SCREEN_HEIGHT = 216
    integer(kind=c_int32_t), target :: pixels(SCREEN_HEIGHT * SCREEN_WIDTH)
    integer, parameter :: map_width=32,map_height=24
    integer, parameter :: mapdata(map_width * map_height) = &
        [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
        1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,3,0,3,0,0,1,1,1,2,1,1,1,1,1,2,1,1,1,2,1,0,0,0,0,0,0,0,0,1,&
        1,0,0,3,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,1,1,1,1,1,&
        1,0,0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,3,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,&
        1,0,0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,1,1,1,1,1,&
        1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,0,0,0,0,0,0,3,3,3,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,&
        1,0,0,0,0,0,0,0,0,3,3,3,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,0,0,0,0,0,0,3,3,3,0,0,3,3,3,0,0,0,0,0,0,0,0,0,3,1,1,1,1,1,&
        1,0,0,0,0,0,0,0,0,3,3,3,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,4,0,0,4,2,0,2,2,2,2,2,2,2,2,0,2,4,4,0,0,4,0,0,0,0,0,0,0,1,&
        1,0,0,4,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,4,0,0,0,0,0,0,0,1,&
        1,0,0,4,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,4,0,0,0,0,0,0,0,1,&
        1,0,0,4,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,4,0,0,0,0,0,0,0,1,&
        1,0,0,4,3,3,4,2,2,2,2,2,2,2,2,2,2,2,2,2,4,3,3,4,0,0,0,0,0,0,0,1,&
        1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
    public

    contains

    type(hit_t) function init_hit()

            init_hit%val = 0
            init_hit%side = 0
            init_hit%pos = vector(0., 0., 0.)

    end function init_hit

    type(vector) function checkCollision(fromX, fromY, toX, toY, radius) result(pos)
        !dev.opera.com/articles/3d-games-with-canvas-and-raycasting-part-2/step-2-collision.htm
        real :: fromX, fromY, toX, toY, radius
        
        integer :: blockX, blockY
        logical :: blockTop, blockBottom, blockLeft, blockRight
        real :: dx, dy

        pos = vector(fromX, fromY, 0.)

        if(toY < 0 .or. toy > map_height .or. toX < 0 .or. tox > map_width)return

        blockX = floor(toX)
        blockY = floor(toY)

        if(isBlocking(blockX, blockY))return

        pos = vector(toX, toY, 0.)

        blockTop = isBlocking(blockX, blockY-1)
        blockBottom = isBlocking(blockX, blockY+1)
        blockLeft = isBlocking(blockX-1, blockY)
        blockRight = isBlocking(blockX+1, blockY)

        if(blockTop .and. toY - blockY < radius)then
            toY = blockY + radius
            pos%y = blockY + radius
        end if

        if(blockBottom .and. blockY+1 - toY < radius)then
            toY = blockY+1 - radius
            pos%y = blockY+1 - radius
        end if

        if(blockLeft .and. toX - blockX < radius)then
            toX = blockX + radius
            pos%x = blockX + radius
        end if

        if(blockRight .and. blockX+1 - toX < radius)then
            toX = blockX+1 - radius
            pos%x = blockX+1 - radius
        end if
        !is tile to the top-left wall
        if(isBlocking(blockX-1,blockY-1) .and. .not.(blockTop .and. blockLeft))then
            dx = toX - blockX
            dy = toY - blockY
            if(dx*dx + dy*dy < radius**2)then
                if(dx*dx > dy*dy)then
                    toX = blockX + radius
                    pos%x = toX
                else
                    toY = blockY + radius
                    pos%y = toY
                end if
            end if
        end if
        !is tile to the top-right wall
        if(isBlocking(blockX+1,blockY-1) .and. .not.(blockTop .and. blockRight))then
            dx = toX - (blockX+1)
            dy = toY - blockY
            if(dx*dx + dy*dy < radius**2)then
                if(dx*dx > dy*dy)then
                    toX = blockX+1 - radius
                    pos%x = toX
                else
                    toY = blockY + radius
                    pos%y = toY
                end if
            end if
        end if
        !is tile to the bottom-left wall
        if(isBlocking(blockX-1,blockY+1) .and. .not.(blockBottom .and. blockLeft))then
            dx = toX - blockX
            dy = toY - (blockY+1)
            if(dx*dx + dy*dy < radius**2)then
                if(dx*dx > dy*dy)then
                    toX = blockX + radius
                    pos%x = toX
                else
                    toY = blockY+1 - radius
                    pos%y = toY
                end if
            end if
        end if
        !is tile to the bottom-right wall
        if(isBlocking(blockX+1,blockY+1) .and. .not.(blockBottom .and. blockRight))then
            dx = toX - (blockX+1)
            dy = toY - (blockY+1)
            if(dx*dx + dy*dy < radius**2)then
                if(dx*dx > dy*dy)then
                    toX = blockX+1 - radius
                    pos%x = toX
                else
                    toY = blockY+1 - radius
                    pos%y = toY
                end if
            end if
        end if

    end function checkCollision

    logical function isBlocking(x,y)
        
        integer, intent(in) :: x, y

        isBlocking = .true.
        if(y < 0 .or. y >= map_height .or. x < 0 .or. x >= map_width)return

        if(mapdata(1 + (y * map_width) + x) /= 0)return
        isBlocking = .false.

    end function isBlocking

    subroutine rotate(rot)

        real, intent(in) :: rot
        type(vector) :: p, d

        d = state%dir
        p = state%plane

        state%dir%x = d%x * cos(rot) - d%y * sin(rot)
        state%dir%y = d%x * sin(rot) + d%y * cos(rot)
        state%plane%x = p%x * cos(rot) - p%y * sin(rot)
        state%plane%y = p%x * sin(rot) + p%y * cos(rot) 

    end subroutine rotate

    subroutine verline(x, y0, y1, colour)
        
        use, intrinsic :: iso_fortran_env, only: int64

        integer, intent(in) :: x, y0, y1
        integer, intent(in) :: colour

        integer :: y

        do y = y0, y1
            buffer%pixels(1+(y * SCREEN_WIDTH) + x) = colour
        end do

    end subroutine verline


    subroutine render(texs)
        use iso_fortran_env, only : real64, int8, int64
        type(texture_t), intent(in) :: texs(4)

        type(vector) :: deltadist, sidedist, pos, dir
        real :: xcam, dperp
        real(kind=real64) :: wallX, step, texPos
        integer :: x, stepx, stepy, iposx, iposy, h, y0, y1, y, texY,texHeight,texWidth
        integer :: texX, drawEnd, drawStart, lineHeight,i
        type(hit_t) :: hit
        integer(kind=c_int32_t) :: pixel
        integer(kind=int8) :: r, g, b

        texHeight = texs(1)%height
        texWidth = texs(1)%width
        
        do x = 0, SCREEN_WIDTH-1
            xcam = (2. * ((x) / real(SCREEN_WIDTH))) - 1.

            dir = vector(state%dir%x + state%plane%x * xcam, state%dir%y + state%plane%y * xcam, 0.0)
            
            pos = state%pos
            ! map position coords
            iposx = int(pos%x)
            iposy = int(pos%y)

            !distance across "voxel"
            deltadist = vector(0.,0.,0.)
            if(abs(dir%x) < 1e-20)then
                deltadist%x = 1e30
            else
                deltadist%x = abs(1. / dir%x)
            end if
            if(abs(dir%y) < 1e-20)then
                deltadist%y = 1e30
            else
                deltadist%y = abs(1. / dir%y)
            end if

            !distance from start pos to first x/y side
            sidedist = vector(0.,0.,0.)
            if(dir%x < 0.0)then
                stepx = -1
                sidedist%x = deltadist%x * (pos%x - iposx)
            else
                stepx = 1
                sidedist%x = deltadist%x * (iposx + 1. - pos%x)
            end if
            if(dir%y < 0.0)then
                stepy = -1
                sidedist%y = deltadist%y * (pos%y - iposy)
            else
                stepy = 1
                sidedist%y = deltadist%y * (iposy + 1. - pos%y)
            end if

            hit = hit_t()
            do while(hit%val == 0.)
                if(sidedist%x < sidedist%y)then
                    sidedist%x = sidedist%x + deltadist%x
                    iposx = iposx + stepx
                    hit%side = 0
                else
                    sidedist%y = sidedist%y + deltadist%y
                    iposy = iposy + stepy
                    hit%side = 1
                end if

                if(iposx < 0)then
                    error stop "iposx"
                end if
                if(iposx > map_width)then
                    error stop "iposx2"
                end if
                if(iposy < 0)then
                    error stop "iposy"
                end if
                if(iposy > map_height)then
                    error stop "iposy2"
                end if
                hit%val = mapdata((iposy * map_width + iposx) +1)
            end do

            !distnace to hit
            if(hit%side == 0)then
                dperp = sidedist%x - deltadist%x
            else
                dperp = sidedist%y - deltadist%y
            end if

            lineHeight = int(SCREEN_HEIGHT / dperp)

            drawStart = -lineHeight/2 + SCREEN_HEIGHT/2
            if(drawStart < 0)drawStart=0
            drawEnd = lineHeight / 2 + SCREEN_HEIGHT / 2
            if(drawEnd >= SCREEN_HEIGHT)drawEnd = SCREEN_HEIGHT - 1

            if(hit%side == 0)then
                wallX = state%pos%y + dperp * dir%y
            else
                wallX = state%pos%x + dperp * dir%x
            end if
            wallX = wallX - floor(wallX)

            texX = int(wallX * real(texWidth,kind=real64))
            if(hit%side == 0 .and. dir%x > 0.)texX = texWidth - texX - 1
            if(hit%side == 1 .and. dir%Y < 0.)texX = texWidth - texX - 1

            step = texHeight / real(lineHeight)
            texPos = (drawStart -(SCREEN_HEIGHT/2) + (lineHeight/2)) * step

            do y = drawStart, drawEnd
                texY = iand(int(texPos), texHeight-1)
                texPos = texPos + step
                pixel = texs(hit%val)%texture_pixels((texY) * texWidth + texX+1)
                if(hit%side == 1) pixel = iand(rshift(pixel, 1),8355711)
                i = 1+(y  * SCREEN_WIDTH) + x
                call sdl_get_rgb(pixel, texs(hit%val)%pixel_format, b,g,r)
                buffer%pixels(i) = sdl_map_rgb(buffer%pixel_format, &
                                               ichar(transfer(r, 'a')),&
                                               ichar(transfer(g, 'a')),&
                                               ichar(transfer(b, 'a')))
            end do

            ! get line height to draw
            h = int(SCREEN_HEIGHT / dperp)
            y0 = max((SCREEN_HEIGHT/2.) - (h / 2.), 0.)
            y1 = min((SCREEN_HEIGHT/2.) + (h / 2.), SCREEN_HEIGHT - 1.)

            call verline(x, 0, y0, black)
            ! call verline(x, y0 ,y1, colour)
            call verline(x, y1, SCREEN_HEIGHT-1, grey)

        end do

    end subroutine render
end module folfenstein
