program main

    use sdl2
    use sdl2_image

    use, intrinsic :: iso_c_binding, only: c_associated, c_null_char, c_loc, c_ptr

    use folfenstein
    use video
    use utils

    use omp_lib, only : omp_get_wtime

    implicit none

    type(sdl_event)      :: event
    integer              :: rc
    integer(kind=c_uint8_t), pointer :: keystate(:)
    real :: movespeed, rotspeed
    character(len=*),parameter :: tex_files(11) = ['res/eagle.png      ', 'res/redbrick.png   ',&
                                                   'res/purplestone.png', 'res/greystone.png  ',&
                                                   "res/bluestone.png  ", "res/mossy.png      ",&
                                                   "res/wood.png       ", "res/colorstone.png ",&
                                                   "res/barrel.png     ", "res/greenlight.png ",&
                                                   "res/pillar.png     "]
    type(texture_t) :: texs(11)
    type(sprite_t) :: sprites(19)
    integer :: counter
    real(kind=real64) :: t0,t1,current_fps, avg_fps, smoothing_factor
    smoothing_factor = 0.99
    avg_fps = -1.

    call init_SDL(state)

    call init_buffer(buffer, state, SCREEN_WIDTH, SCREEN_HEIGHT)
    call set_colours(buffer)

    call read_textures(texs, tex_files, state)

    sprites = [sprite_t(11.5, 20.5, 10),&
               sprite_t(4.5, 18.5, 10),&
               sprite_t(4.5, 10., 10),&
               sprite_t(12.5, 105, 10),&
               sprite_t(6.5, 3.5, 10),&
               sprite_t(20.5, 3.5, 10),&
               sprite_t(14.5, 4.5, 10),&
               sprite_t(20.5, 14.5, 10),&
               !pillars
               sprite_t(10.5, 18.5, 11),&
               sprite_t(11.5, 18.5, 11),&
               sprite_t(12.5, 18.5, 11),&
               !barrels
               sprite_t(1.8, 21.5, 9),&
               sprite_t(1.5, 15.5, 9),&
               sprite_t(1.8, 16., 9),&
               sprite_t(1.2, 16.2, 9),&
               sprite_t(2.5, 3.5, 9),&
               sprite_t(15.5, 9.5, 9),&
               sprite_t(15.1, 10.0, 9),&
               sprite_t(15.8, 10.5, 9)]

    rotspeed =  3.0 * 0.016
    movespeed = 3.0 * 0.016

    state%pos = vector(11.5, 20, 0.)
    state%dir = vector(-1., 0.0, 0.0)
    state%dir = state%dir%magnitude()
    state%plane = vector(0.0, 0.66, 0.0)

    counter = 0
    t0 = 0.
    t1 = 0.

    t0 =omp_get_wtime()
    do while(state%is_running)
        ! Catch events.
        do while (sdl_poll_event(event) > 0)
            select case (event%type)
                case (SDL_QUITEVENT)
                    state%is_running = .false.
            end select
        end do

        keystate(0:) => sdl_get_keyboard_state()
        if(keystate(SDL_SCANCODE_ESCAPE) == 1)then
            state%is_running = .false.
            exit
        end if
        if(keystate(SDL_SCANCODE_LEFT) == 1)then
            call rotate(rotspeed)
        end if

        if(keystate(SDL_SCANCODE_RIGHT) == 1)then
            call rotate(-1.*rotspeed)
        end if

        if(keystate(SDL_SCANCODE_UP) == 1)then
            state%pos = checkCollision(state%pos%x,state%pos%y,state%pos%x + (state%dir%x * movespeed),&
                                      state%pos%y + (state%dir%y * movespeed),0.1)
        end if
        if(keystate(SDL_SCANCODE_DOWN) == 1)then
            state%pos = checkCollision(state%pos%x,state%pos%y,state%pos%x - (state%dir%x * movespeed),&
                                       state%pos%y - (state%dir%y * movespeed),0.1)
        end if
        
        rc = sdl_lock_texture(buffer%texture, buffer%rect, buffer%pixels_ptr, buffer%pitch)
            call render(texs)
            call render_sprites(sprites, texs)
        call sdl_unlock_texture(buffer%texture)

        rc = sdl_render_copy(state%renderer, buffer%texture, buffer%rect, state%screen_rect)
        call sdl_render_present(state%renderer)
        t1 = omp_get_wtime()

        ! current_fps = 1. / (t1-t0)
        ! t0 = t1
        ! if(avg_fps < 0)avg_fps = current_fps
        ! avg_fps = (avg_fps * smoothing_factor) + current_fps * (1.-smoothing_factor)
        ! print*,avg_fps
    end do

    ! Quit gracefully.
    buffer%pixels => null()
    call sdl_destroy_texture(buffer%texture)
    call sdl_destroy_renderer(state%renderer)
    call sdl_destroy_window(state%window)
    call sdl_quit()
end program main
