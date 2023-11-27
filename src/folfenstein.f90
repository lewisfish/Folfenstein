module folfenstein

    use, intrinsic :: iso_c_binding,   only : c_int64_t, c_ptr, c_int, c_int32_t
    use, intrinsic :: iso_fortran_env, only : real64
    use sdl2
    
    use utils
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

    type :: sprite_t
        real(kind=real64) :: x, y
        integer :: texture
    end type sprite_t

    type(buffer_t) :: buffer

    type(state_t) :: state
    integer :: spriteOrder(19)
    real(kind=real64) :: spriteDistance(19)

    integer(kind=c_int), parameter :: SCREEN_WIDTH  = 384*2
    integer(kind=c_int), parameter :: SCREEN_HEIGHT = 216*2
    integer(kind=c_int32_t), target :: pixels(SCREEN_HEIGHT * SCREEN_WIDTH)
    integer, parameter :: map_width=24,map_height=24
    integer, parameter :: mapdata(map_width * map_height) = &
        ! [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
        ! 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,3,0,3,0,0,1,1,1,2,1,1,1,1,1,2,1,1,1,2,1,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,3,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,1,1,1,1,1,&
        ! 1,0,0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,3,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,&
        ! 1,0,0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,1,1,1,1,1,&
        ! 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,0,0,0,0,0,0,3,3,3,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,&
        ! 1,0,0,0,0,0,0,0,0,3,3,3,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,0,0,0,0,0,0,3,3,3,0,0,3,3,3,0,0,0,0,0,0,0,0,0,3,1,1,1,1,1,&
        ! 1,0,0,0,0,0,0,0,0,3,3,3,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,4,0,0,4,2,0,2,2,2,2,2,2,2,2,0,2,4,4,0,0,4,0,0,0,0,0,0,0,1,&
        ! 1,0,0,4,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,4,0,0,0,0,0,0,0,1,&
        ! 1,0,0,4,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,4,0,0,0,0,0,0,0,1,&
        ! 1,0,0,4,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,4,0,0,0,0,0,0,0,1,&
        ! 1,0,0,4,3,3,4,2,2,2,2,2,2,2,2,2,2,2,2,2,4,3,3,4,0,0,0,0,0,0,0,1,&
        ! 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
        ! 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
        [8,8,8,8,8,8,8,8,8,8,8,4,4,6,4,4,6,4,6,4,4,4,6,4,&
        8,0,0,0,0,0,0,0,0,0,8,4,0,0,0,0,0,0,0,0,0,0,0,4,&
        8,0,3,3,0,0,0,0,0,8,8,4,0,0,0,0,0,0,0,0,0,0,0,6,&
        8,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,&
        8,0,3,3,0,0,0,0,0,8,8,4,0,0,0,0,0,0,0,0,0,0,0,4,&
        8,0,0,0,0,0,0,0,0,0,8,4,0,0,0,0,0,6,6,6,0,6,4,6,&
        8,8,8,8,0,8,8,8,8,8,8,4,4,4,4,4,4,6,0,0,0,0,0,6,&
        7,7,7,7,0,7,7,7,7,0,8,0,8,0,8,0,8,4,0,4,0,6,0,6,&
        7,7,0,0,0,0,0,0,7,8,0,8,0,8,0,8,8,6,0,0,0,0,0,6,&
        7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,6,0,0,0,0,0,4,&
        7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,6,0,6,0,6,0,6,&
        7,7,0,0,0,0,0,0,7,8,0,8,0,8,0,8,8,6,4,6,0,6,6,6,&
        7,7,7,7,0,7,7,7,7,8,8,4,0,6,8,4,8,3,3,3,0,3,3,3,&
        2,2,2,2,0,2,2,2,2,4,6,4,0,0,6,0,6,3,0,0,0,0,0,3,&
        2,2,0,0,0,0,0,2,2,4,0,0,0,0,0,0,4,3,0,0,0,0,0,3,&
        2,0,0,0,0,0,0,0,2,4,0,0,0,0,0,0,4,3,0,0,0,0,0,3,&
        1,0,0,0,0,0,0,0,1,4,4,4,4,4,6,0,6,3,3,0,0,0,3,3,&
        2,0,0,0,0,0,0,0,2,2,2,1,2,2,2,6,6,0,0,5,0,5,0,5,&
        2,2,0,0,0,0,0,2,2,2,0,0,0,2,2,0,5,0,5,0,0,0,5,5,&
        2,0,0,0,0,0,0,0,2,0,0,0,0,0,2,5,0,5,0,5,0,5,0,5,&
        1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,&
        2,0,0,0,0,0,0,0,2,0,0,0,0,0,2,5,0,5,0,5,0,5,0,5,&
        2,2,0,0,0,0,0,2,2,2,0,0,0,2,2,0,5,0,5,0,0,0,5,5,&
        2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,5,5,5,5,5,5,5,5,5]

    public

    contains

    subroutine sortSprites(order, dist, amount)

        integer :: order(:), amount
        real(kind=real64) :: dist(:)

        type(pair) :: sprites(amount)
        integer :: i

        do i = 1, amount
            sprites(i)%first = dist(i)
            sprites(i)%second = order(i)
        end do

        call sort_pairs(sprites)

        do i = 0, amount-1
            dist(i+1) = sprites(amount - i)%first
            order(i+1) = sprites(amount - i)%second
        end do

    end subroutine sortSprites

    subroutine render_sprites(sprites, texs)
        use iso_fortran_env, only : int8

        type(sprite_t) :: sprites(:)
        type(texture_t) :: texs(:)
        
        integer :: i, spriteScreenX, spriteHeight, drawStartY, drawEndY, drawStartX, spriteWidth, drawEndX, stripe
        integer :: texWidth, texHeight, texX, texY, y, d, p
        real(kind=real64) :: invDet, spriteX, spriteY, transformX, transformY
        integer(kind=c_int32_t) :: pixel

        texHeight = texs(1)%height
        texWidth = texs(1)%width

        do i = 1, size(sprites)
            spriteOrder(i) = i
            spriteDistance(i) = ((state%pos%X - sprites(i)%x) * (state%pos%X - sprites(i)%x) +&
                                 (state%pos%Y - sprites(i)%y) * (state%pos%Y - sprites(i)%y))
        end do

        call sortSprites(spriteOrder, spriteDistance, 19)

        !$omp parallel do default(private) shared(buffer,state,texs,spriteOrder,sprites,texheight,texwidth)
        do i = 1, size(sprites)
            spriteX = sprites(spriteOrder(i))%x - state%pos%X
            spriteY = sprites(spriteOrder(i))%y - state%pos%Y

            invDet = 1. / (state%plane%X *state%dir%Y - state%dir%X*state%plane%y)
            transformX = invDet * (state%dir%Y *spriteX - state%dir%X*spriteY)
            transformY = invDet * (-state%plane%y *spriteX + state%plane%X*spriteY)

            spriteScreenX = int((SCREEN_WIDTH / 2) * (1 + transformX/transformY))

            spriteHeight = abs(int(SCREEN_HEIGHT / (transformY)))
            drawStartY = -spriteHeight/2 + SCREEN_HEIGHT/2
            if(drawStartY < 0)drawStartY=0
            drawEndY = spriteHeight/2 + spriteScreenX
            if(drawEndY >= SCREEN_HEIGHT)drawEndY= SCREEN_HEIGHT-1

            spriteWidth = abs(int(SCREEN_HEIGHT / transformY))
            drawStartX = -spriteWidth / 2 + spriteScreenX
            if(drawStartX < 0)drawStartX = 0
            drawEndX = spriteWidth/2 + spriteScreenX
            if(drawEndX >= SCREEN_WIDTH)drawEndX=SCREEN_WIDTH-1

            do stripe = drawStartX, drawEndX-1
                texX = int(256 * (stripe - (-spriteWidth/2 + spriteScreenX)) * texWidth/spriteWidth)/256
                if(transformY > 0 .and. stripe > 0 .and. stripe < SCREEN_WIDTH .and. transformY < state%ZBuffer(stripe+1))then
                    do y = drawStartY, drawEndY
                        d = y * 256 - SCREEN_HEIGHT * 128 + spriteHeight * 128
                        texY = (((d * texHeight) / spriteHeight) / 256) 

                        p=min(texWidth * texY + texX, 4096)
                        pixel = texs(sprites(spriteOrder(i))%texture)%texture_pixels(p)
                        if(iand(pixel, z"00FFFFFF") /= 0)then
                        p = 1+(y  * SCREEN_WIDTH) + stripe

                        buffer%pixels(p) = pixel
                        end if
                    end do
                end if
            end do
        end do
    end subroutine render_sprites

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
        type(texture_t), intent(in) :: texs(:)

        type(vector) :: deltadist, sidedist, pos, dir
        real :: xcam, dperp
        real(kind=real64) :: wallX, step, texPos
        integer :: x, stepx, stepy, iposx, iposy, h, y0, y1, y, texY,texHeight,texWidth
        integer :: texX, drawEnd, drawStart, lineHeight,i
        type(hit_t) :: hit
        integer(kind=c_int32_t) :: pixel

        texHeight = texs(1)%height
        texWidth = texs(1)%width

        !$omp parallel do default(private) shared(buffer, black,grey, texheight,texwidth,texs,state)
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
                buffer%pixels(i) = pixel
            end do

            ! get line height to draw
            h = int(SCREEN_HEIGHT / dperp)
            y0 = max((SCREEN_HEIGHT/2.) - (h / 2.), 0.)
            y1 = min((SCREEN_HEIGHT/2.) + (h / 2.), SCREEN_HEIGHT - 1.)

            call verline(x, 0, y0, black)
            ! call verline(x, y0 ,y1, colour)
            call verline(x, y1, SCREEN_HEIGHT-1, grey)
            state%ZBuffer(x+1) = dperp
        end do
!$omp end parallel do
    end subroutine render
end module folfenstein
