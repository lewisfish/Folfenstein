module video

    use, intrinsic :: iso_c_binding,   only : c_ptr, c_null_char, c_associated, c_int32_t, c_f_pointer, c_int8_t
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    
    use sdl2, only : sdl_init, sdl_init_video, sdl_get_error, sdl_create_window,sdl_create_renderer,&
                     SDL_WINDOWPOS_UNDEFINED, SDL_WINDOW_ALLOW_HIGHDPI,SDL_RENDERER_PRESENTVSYNC,&
                     sdl_pixel_format, sdl_rect, SDL_PIXELFORMAT_ABGR8888, SDL_TEXTUREACCESS_STREAMING,&
                     sdl_create_texture, sdl_get_window_pixel_format, sdl_alloc_format, sdl_map_rgb,&
                     sdl_lock_texture, sdl_unlock_texture, sdl_surface, sdl_create_texture_from_surface,&
                     sdl_query_texture,sdl_get_rgb,SDL_Convert_Surface
    use sdl2_image, only : img_load, img_init, IMG_INIT_PNG

    use vector_class

    implicit none
    
    type :: state_t
        type(c_ptr)    :: window, renderer
        type(sdl_rect) :: screen_rect
        logical        :: is_running
        type(vector)   :: pos, dir, plane
    end type state_t

    type :: buffer_t
        integer                          :: format       ! Texture format.
        integer                          :: pitch        ! Texture pitch.
        integer(kind=c_int32_t), pointer :: pixels(:)    ! SDL_Texture pixels pointer.
        type(c_ptr)                      :: pixels_ptr   ! C pointer to texture pixels.
        type(c_ptr)                      :: texture      ! C pointer to SDL_Texture.
        type(sdl_pixel_format),  pointer :: pixel_format ! SDL_PixelFormat of SDL_Texture.
        type(sdl_rect)                   :: rect         ! Utitlity rectangle.
    end type buffer_t

    type :: texture_t
        integer                          :: access              ! Texture access.
        integer                          :: format              ! Texture format.
        integer                          :: pitch               ! Texture pitch.
        integer                          :: width               ! Texture width.
        integer                          :: height              ! Texture height.
        integer(kind=c_int8_t),  pointer :: surface_pixels(:)   ! SDL_Surface pixels pointer.
        integer(kind=c_int32_t), pointer :: texture_pixels(:)   ! SDL_Texture pixels pointer.
        type(c_ptr)                      :: pixels_ptr          ! C pointer to texture pixels.
        type(c_ptr)                      :: texture             ! C pointer to SDL_Texture.
        type(sdl_pixel_format),  pointer :: pixel_format        ! SDL_PixelFormat of SDL_Texture.
        type(sdl_surface),       pointer :: surface             ! SDL_Surface pointer.
    end type texture_t

    integer :: red, green, blue, black, grey, pink

    private
    public :: state_t, init_SDL, buffer_t, init_buffer, set_colours
    public :: red, green, blue, black, grey, pink, texture_t, read_textures

contains
    
    subroutine init_SDL(state)

        type(state_t), intent(inout) :: state

        ! Initialise SDL.
        if (sdl_init(SDL_INIT_VIDEO) < 0) then
            write(stderr, '("SDL Error: ", a)') sdl_get_error()
            stop
        end if
    
        ! Create the SDL window.
        state%window = sdl_create_window('Folfenstein 3D' // c_null_char, &
                                   SDL_WINDOWPOS_UNDEFINED, &
                                   SDL_WINDOWPOS_UNDEFINED, &
                                   1280, &
                                   720, &
                                   SDL_WINDOW_ALLOW_HIGHDPI)
    
        if (.not. c_associated(state%window)) then
            write (stderr, '("SDL Error: ", a)') sdl_get_error()
            stop
        end if

        state%renderer = sdl_create_renderer(state%window, -1, SDL_RENDERER_PRESENTVSYNC)
        state%screen_rect = sdl_rect(0, 0, 1280, 720)
        state%is_running = .true.

    end subroutine init_SDL

    subroutine init_buffer(buffer, state, w, h)

        type(buffer_t), intent(inout) :: buffer
        type(state_t),  intent(in)    :: state
        integer,        intent(in)    :: w, h

        integer :: rc

        buffer%texture = sdl_create_texture(state%renderer, &
                                            SDL_PIXELFORMAT_ABGR8888, &
                                            SDL_TEXTUREACCESS_STREAMING, &
                                            w, h)

        buffer%format = sdl_get_window_pixel_format(state%window)
        buffer%pixel_format => sdl_alloc_format(buffer%format)

        ! Set position and size of the rectangle.
        buffer%rect = sdl_rect(0, 0, w, h)

        rc = sdl_lock_texture(buffer%texture, buffer%rect, buffer%pixels_ptr, buffer%pitch)
        call c_f_pointer(buffer%pixels_ptr, buffer%pixels, shape=[w*h])
        call sdl_unlock_texture(buffer%texture)

    end subroutine init_buffer

    subroutine set_colours(buffer)
  
        type(buffer_t), intent(in) :: buffer

        black = sdl_map_rgb(buffer%pixel_format, 0, 0, 0)
        grey = sdl_map_rgb(buffer%pixel_format, 50, 50, 50)
        red = sdl_map_rgb(buffer%pixel_format, 0, 0, 255)
        green = sdl_map_rgb(buffer%pixel_format, 0, 255, 0)
        blue = sdl_map_rgb(buffer%pixel_format, 255, 0, 0)
        pink = sdl_map_rgb(buffer%pixel_format, 255, 0, 255)

    end subroutine set_colours

    subroutine read_textures(texs, filenames, state)

        use iso_fortran_env, only : int8
        
        character(*),    intent(in) :: filenames(4)
        type(texture_t), intent(inout) :: texs(4)
        type(state_t),   intent(in) :: state
        
        type(sdl_pixel_format) :: fmt
        integer :: i, rc

        do i = 1, 4
            texs(i)%surface => IMG_Load(filenames(i) // c_null_char)

            fmt = sdl_alloc_format(SDL_PIXELFORMAT_ABGR8888)
            texs(i)%surface = SDL_Convert_Surface(texs(i)%surface, fmt, 0)
            call c_f_pointer(texs(i)%surface%format, texs(i)%pixel_format)

            texs(i)%texture = sdl_create_texture_from_surface(state%renderer, texs(i)%surface)
            rc = sdl_query_texture(texs(i)%texture, texs(i)%format, texs(i)%access, texs(i)%width, texs(i)%height)        
            call c_f_pointer(texs(i)%surface%pixels, texs(i)%texture_pixels, shape=[texs(i)%surface%w * texs(i)%surface%h])
            call c_f_pointer(texs(i)%surface%pixels, texs(i)%surface_pixels, shape=[texs(i)%surface%w * texs(i)%surface%h])
        end do
    end subroutine read_textures
end module video