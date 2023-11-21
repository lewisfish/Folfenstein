program main

    use sdl2
    use sdl2_image

    use, intrinsic :: iso_c_binding, only: c_associated, c_null_char, c_loc, c_ptr

    use folfenstein
    use video

    implicit none

    type(sdl_event)      :: event
    integer              :: rc
    integer(kind=c_uint8_t), pointer :: keystate(:)
    real :: movespeed, rotspeed
    character(len=*),parameter :: FILE_NAME(4) = ['res/walls-1.png', 'res/walls-2.png',&
                                                  'res/walls-3.png', 'res/walls-4.png']
    type(texture_t) :: texs(4)
    
    call init_SDL(state)

    call init_buffer(buffer, state, SCREEN_WIDTH, SCREEN_HEIGHT)
    call set_colours(buffer)

    call read_textures(texs, FILE_NAME, state)

   
    rotspeed =  3.0 * 0.016
    movespeed = 3.0 * 0.016

    state%pos = vector(2., 2., 0.)
    state%dir = vector(-1., 0.1, 0.0)
    state%dir = state%dir%magnitude()
    state%plane = vector(0.0, 0.66, 0.0)

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
        call sdl_unlock_texture(buffer%texture)

        rc = sdl_render_copy(state%renderer, buffer%texture, buffer%rect, state%screen_rect)
        call sdl_render_present(state%renderer)
    end do

    ! Quit gracefully.
    buffer%pixels => null()
    call sdl_destroy_texture(buffer%texture)
    call sdl_destroy_renderer(state%renderer)
    call sdl_destroy_window(state%window)
    call sdl_quit()
end program main
