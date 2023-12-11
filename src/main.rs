use bevy::{
    ecs::event::event_update_system,
    prelude::*,
    sprite::{collide_aabb::collide, MaterialMesh2dBundle},
    ui::FocusPolicy,
};
use bevy_jam_4::line_marker::{LineMarker, LineMaterial};

// TODO: destroy whole enemy and ship if base is destroyed
// TODO: enenmy types
// TODO: replay
// TODO: title screen
// TODO: level progress bar

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, MaterialPlugin::<LineMaterial>::default()))
        .insert_resource(PauseState(false))
        .insert_resource(GunTick(Timer::from_seconds(0.4, TimerMode::Repeating)))
        .add_event::<NewLevel>()
        .add_event::<Unpaused>()
        .insert_resource(LevelTick(Timer::from_seconds(1.0, TimerMode::Repeating)))
        .insert_resource(Health(20))
        .add_systems(PreStartup, (setup, apply_deferred, setup_ui).chain())
        .add_systems(Startup, (Ship::create, Enemy::setup))
        .add_systems(Update, event_update_system::<NewLevel>)
        .add_systems(PreUpdate, ship_ui_manager)
        .add_systems(
            Update,
            (
                Ship::r#move,
                Gun::shoot,
                Level::constant_increase,
                mr_god,
                health_bar_update,
                move_seekers,
                take_damage,
                kill_enemies,
                pause,
                show_new_level,
                hide_new_level,
            ),
        )
        .add_systems(PostUpdate, update_level_ui)
        .run();
}

#[derive(Event)]
struct Unpaused;

#[derive(Resource)]
struct PauseState(bool);

fn pause(
    mut paused: ResMut<PauseState>,
    input: Res<Input<KeyCode>>,
    mut events: EventWriter<Unpaused>,
) {
    if input.just_pressed(KeyCode::Space) {
        if paused.0 {
            paused.0 = false;
            events.send(Unpaused);
        } else {
            paused.0 = true;
        }
    }
}

fn show_new_level(
    mut events: EventReader<NewLevel>,
    mut new_level_popup: Query<&mut Visibility, With<NewLevelPopup>>,
    mut ui_background_color: Query<&mut BackgroundColor, With<ShipUi>>,
    mut paused: ResMut<PauseState>,
) {
    if events.is_empty() {
        return;
    }

    events.clear();
    paused.0 = true;
    for mut visibility in new_level_popup.iter_mut() {
        *visibility = Visibility::Visible;
    }

    let mut background_color = ui_background_color.single_mut();
    background_color.0 = Color::YELLOW;
}

fn hide_new_level(
    mut events: EventReader<Unpaused>,
    mut new_level_popup: Query<&mut Visibility, With<NewLevelPopup>>,
    mut ui_background_color: Query<&mut BackgroundColor, With<ShipUi>>,
) {
    if events.is_empty() {
        return;
    }

    events.clear();
    for mut visibility in new_level_popup.iter_mut() {
        *visibility = Visibility::Hidden;
    }

    let mut background_color = ui_background_color.single_mut();
    background_color.0 = NORMAL_BUTTON;
}

#[derive(Component, Default)]
struct Gun {
    direction: Vec3,
    bullet_speed: f32,
}

impl Gun {
    fn shoot(
        mut commands: Commands,
        time: Res<Time>,
        mut gun_timer: ResMut<GunTick>,
        animation: Res<GunAnimation>,

        paused: Res<PauseState>,

        mut query: Query<(&GlobalTransform, &mut AnimationPlayer, &Gun, Option<&Enemy>)>,
    ) {
        if paused.0 {
            return;
        }
        if !gun_timer.0.tick(time.delta()).just_finished() {
            return;
        }
        for (g_pos, mut player, gun, is_enemy) in query.iter_mut() {
            let mut bullet = commands.spawn_empty();
            bullet.insert((
                Bullet {
                    direction: gun.direction,
                    speed: gun.bullet_speed,
                },
                Box::from_transform(g_pos.compute_transform()),
            ));

            if is_enemy.is_some() {
                bullet.insert(Enemy {});
            }

            player.start(animation.0.clone_weak());
        }
    }
}

#[derive(Bundle)]
struct GunName {
    name: Name,
}

impl Default for GunName {
    fn default() -> Self {
        Self {
            name: Name::new("gun"),
        }
    }
}

#[derive(Bundle, Default)]
struct GunBundle {
    gun: Gun,
    animation: AnimationPlayer,
    name: GunName,
}

#[derive(Component)]
struct Bullet {
    direction: Vec3,
    speed: f32,
}

#[derive(Resource)]
struct GunAnimation(Handle<AnimationClip>);

#[derive(Resource)]
struct GunTick(Timer);

#[derive(Component, Debug, Clone, Copy)]
struct GridPosition(Vec2);

impl GridPosition {
    const SIZE: f32 = 10.0;
}

fn grid_to_transfrom(pos: IVec2) -> Transform {
    Transform::from_translation(Vec3::new(pos.x as f32, pos.y as f32, 0.) * GridPosition::SIZE)
}

#[derive(Bundle)]
struct Box {
    sprite: SpriteBundle,
}

impl Box {
    pub fn from_grid(x: f32, y: f32) -> Self {
        let transform = Transform::from_translation(Vec3::new(x, y, 0.) * GridPosition::SIZE);
        Self::from_transform(transform)
    }

    pub fn from_grid_vec2(pos: Vec2) -> Self {
        Self::from_grid(pos.x, pos.y)
    }

    pub fn from_transform(transform: Transform) -> Self {
        Self {
            sprite: SpriteBundle {
                sprite: Sprite {
                    color: Color::rgb(0.25, 0.25, 0.25),
                    custom_size: Some(Vec2::splat(GridPosition::SIZE)),
                    ..Default::default()
                },
                transform,
                ..default()
            },
        }
    }

    pub fn color(mut self, color: Color) -> Self {
        self.sprite.sprite.color = color;
        self
    }
}

#[derive(Resource)]
struct Health(usize);

#[derive(Debug, Clone, Copy, Default, PartialEq)]
enum PartType {
    #[default]
    None,
    Base,
    Gun(Vec2),
    Connector,
    Shield,
}

impl PartType {
    fn color(&self) -> Color {
        match self {
            PartType::None => NORMAL_BUTTON,
            PartType::Base | PartType::Shield => Color::rgb(0.25, 0.25, 0.25),
            PartType::Connector => Color::rgb(0.45, 0.45, 0.45),
            PartType::Gun(_) => Color::rgb(0.35, 0.35, 0.35),
        }
    }
}

#[derive(Component)]
struct Ship {
    previous_position: Vec3,
    speed: f32,
    angular_speed: f32,
}

impl Ship {
    const MAX_WIDTH: u8 = 15;
    const MAX_HEIGHT: u8 = 11;

    fn create(
        mut commands: Commands,
        mut ship_commands: Commands,

        sprites: Res<PartSprites>,

        ship_ui: Query<Entity, With<ShipUi>>,
    ) {
        let mut ship_ui = ship_commands
            .get_entity(ship_ui.single())
            .expect("ui should be created before ship");

        let mut ship_pos = SpatialBundle::INHERITED_IDENTITY;
        let position = Vec3::X * GridPosition::SIZE * -20. + Vec3::Y * GridPosition::SIZE * -5.;
        ship_pos.transform = ship_pos.transform.with_translation(position);
        commands
            .spawn((
                Ship {
                    previous_position: position,
                    speed: 200.,
                    angular_speed: 10.,
                },
                Level {
                    level: 0,
                    progress: 0.,
                },
                ship_pos,
            ))
            .with_children(|parent| {
                for y in (0..Ship::MAX_HEIGHT).rev() {
                    for x in 0..Ship::MAX_WIDTH {
                        let offset_from_base = IVec2 {
                            x: (x as i32 - (Ship::MAX_WIDTH / 2) as i32),
                            y: (y as i32 - (Ship::MAX_HEIGHT / 2) as i32),
                        };

                        let (visibility, part_type) = match offset_from_base.to_array() {
                            [0, 0] => (Visibility::Visible, PartType::Base),
                            [0, 1] => (Visibility::Visible, PartType::Gun(Vec2::new(0., 1.))),
                            _ => (Visibility::Hidden, PartType::None),
                        };
                        let mut ship_part = parent.spawn((
                            ShipPart {
                                grid_pos: offset_from_base,
                            },
                            SpriteBundle {
                                sprite: Sprite {
                                    color: part_type.color(),
                                    custom_size: Some(Vec2::splat(GridPosition::SIZE)),
                                    ..Default::default()
                                },
                                texture: sprites.for_part(part_type),
                                transform: grid_to_transfrom(offset_from_base),
                                visibility,
                                ..Default::default()
                            },
                        ));
                        if let PartType::Gun(_) = part_type {
                            ship_part.insert(GunBundle {
                                gun: Gun {
                                    direction: offset_from_base
                                        .as_vec2()
                                        .normalize()
                                        .round()
                                        .extend(0.),
                                    bullet_speed: 2.,
                                },
                                ..Default::default()
                            });
                        }

                        ship_ui.with_children(|ui| {
                            ui.spawn((
                                ButtonBundle {
                                    style: Style {
                                        border: UiRect::all(Val::Px(2.)),
                                        ..Default::default()
                                    },
                                    image: sprites.for_part(part_type).into(),
                                    background_color: part_type.color().into(),
                                    border_color: part_type.color().into(),
                                    ..Default::default()
                                },
                                ShipPartUi {
                                    ship_part: ship_part.id(),
                                    part_type,
                                },
                            ));
                        });
                    }
                }
            });
    }

    fn r#move(
        time: Res<Time>,
        key: Res<Input<KeyCode>>,
        paused: Res<PauseState>,
        mut gizmos: Gizmos,

        mut query: Query<(&mut Transform, &mut Ship)>,
        mut guns: Query<&mut Gun, With<ShipPart>>,
    ) {
        if paused.0 {
            return;
        }

        let (mut transform, mut ship) = query.single_mut();
        let mut dir = Vec2::new(0., 0.);
        if key.pressed(KeyCode::W) {
            dir.y += 1.;
        }
        if key.pressed(KeyCode::S) {
            dir.y -= 1.;
        }
        if key.pressed(KeyCode::A) {
            dir.x -= 1.;
        }
        if key.pressed(KeyCode::D) {
            dir.x += 1.;
        }

        if dir.length() > 0. {
            ship.previous_position = transform.translation;
            transform.translation.x += dir.x * ship.speed * time.delta_seconds();
            transform.translation.y += dir.y * ship.speed * time.delta_seconds();
        } else {
            ship.previous_position = transform.translation;
        }

        // if rotate_timer.0.tick(time.delta()).just_finished() {
        let mut rotate = 0f32;
        if key.pressed(KeyCode::Left) {
            info!(rotate, "pressed left!");
            rotate += 1.;
        }
        if key.pressed(KeyCode::Right) {
            info!(rotate, "pressed right!");
            rotate -= 1.;
        }
        let angle = rotate * time.delta_seconds() * ship.angular_speed;

        transform.rotate_z(angle);
        let r = Vec2::from_angle(angle);
        for mut gun in guns.iter_mut() {
            let new_direction = r.rotate(gun.direction.xy());
            gun.direction.x = new_direction.x;
            gun.direction.y = new_direction.y;
        }
    }
}

#[derive(Resource)]
struct LevelTick(Timer);

#[derive(Component)]
struct Level {
    level: u32,
    progress: f32,
}

impl Level {
    fn add(&mut self, progress: f32) -> Option<NewLevel> {
        self.progress += progress;
        if self.progress > 1. {
            self.level += 1;
            self.progress -= 1.;
            Some(NewLevel)
        } else {
            None
        }
    }

    fn add_with_event(&mut self, event: &mut EventWriter<NewLevel>, progress: f32) {
        if let Some(nl) = self.add(progress) {
            event.send(nl);
        }
    }

    fn constant_increase(
        time: Res<Time>,
        paused: Res<PauseState>,
        mut event: EventWriter<NewLevel>,
        mut timer: ResMut<LevelTick>,
        mut level: Query<&mut Level>,
    ) {
        if paused.0 {
            return;
        }
        if !timer.0.tick(time.delta()).just_finished() {
            return;
        }
        level.single_mut().add_with_event(&mut event, 0.05);
    }
}

#[derive(Event)]
struct NewLevel;

#[derive(Component, Debug)]
struct ShipPart {
    grid_pos: IVec2,
}

/// Pan Bóg kule nosi
fn mr_god(paused: Res<PauseState>, mut bullets: Query<(&Bullet, &mut Transform)>) {
    if paused.0 {
        return;
    }
    for (bullet, mut transform) in bullets.iter_mut() {
        transform.translation.x += bullet.direction.x * bullet.speed;
        transform.translation.y += bullet.direction.y * bullet.speed;
    }
}

fn take_damage(
    mut commands: Commands,
    mut health: ResMut<Health>,
    enemies: Query<(Entity, Option<&mut Parent>, &Enemy, &GlobalTransform)>, // TODO: move unused to With<T>
    players: Query<(Entity, &ShipPart, &Visibility, &GlobalTransform)>,
) {
    let mut killed = false;
    for (enemy_id, parent, _, enemy_transform) in enemies.iter() {
        let enemy_transform = enemy_transform.compute_transform();

        for (_, _, visibility, player) in players.iter() {
            if visibility == Visibility::Hidden {
                continue;
            }
            let player_transform = player.compute_transform();
            if let Some(_) = collide(
                enemy_transform.translation,
                Vec2::splat(GridPosition::SIZE),
                player_transform.translation,
                Vec2::splat(GridPosition::SIZE),
            ) {
                // despawn the parent (if it exists) or the enemy itself
                commands
                    .entity(parent.map(|p| p.get()).unwrap_or(enemy_id))
                    .despawn_recursive();

                match health.0.checked_sub(1) {
                    Some(new_health) => health.0 = new_health,
                    None => killed = true,
                }
            }
        }
    }

    if killed {
        for (id, _, _, _) in players.iter() {
            commands.entity(id).despawn();
        }
    }
}

fn kill_enemies(
    mut commands: Commands,

    mut event: EventWriter<NewLevel>,
    mut level: Query<&mut Level>,

    enemies: Query<(Entity, &Enemy, &GlobalTransform, Has<Bullet>)>,
    friendly_bullets: Query<(Entity, &Bullet, &GlobalTransform), Without<Enemy>>,
) {
    for (enemy_id, _, enemy, is_bullet) in enemies.iter() {
        let enemy = enemy.compute_transform();
        for (bullet_id, _, bullet) in friendly_bullets.iter() {
            let bullet = bullet.compute_transform();
            if let Some(_) = collide(
                enemy.translation,
                Vec2::splat(GridPosition::SIZE),
                bullet.translation,
                Vec2::splat(GridPosition::SIZE),
            ) {
                if !is_bullet {
                    for mut level in level.iter_mut() {
                        level.add_with_event(&mut event, 0.25);
                    }
                }
                commands.entity(enemy_id).despawn();
                commands.entity(bullet_id).despawn();
            }
        }
    }
}

#[derive(Component)]
struct Enemy {}

#[derive(Bundle)]
struct EnemyBundle {
    enemy: Enemy,
    body: Box,
}

impl Enemy {
    fn setup(
        mut commands: Commands,
        mut meshes: ResMut<Assets<Mesh>>,
        mut materials: ResMut<Assets<LineMaterial>>,
    ) {
        let direction = Vec3::new(0., -1., 0.);
        let position = Vec3::X * GridPosition::SIZE * 20. + Vec3::Y * GridPosition::SIZE * 5.;
        commands.spawn(MaterialMesh2dBundle {
            mesh: meshes
                .add(Mesh::from(LineMarker {
                    start: Vec3::ZERO,
                    end: Vec3::new(3., 0., 0.),
                }))
                .into(),
            transform: Transform::from_xyz(0.5, 0.0, 0.0),
            material: materials.add(LineMaterial { color: Color::BLUE }),
            ..default()
        });
        commands
            .spawn((
                Seeker {
                    velocity: direction,
                    prediction_mult: 1,
                },
                MaterialMeshBundle {
                    mesh: meshes.add(Mesh::from(LineMarker {
                        start: Vec3::ZERO,
                        end: direction * GridPosition::SIZE * 2.,
                    })),
                    material: materials.add(LineMaterial { color: Color::BLUE }),
                    transform: Transform::from_translation(position),
                    ..Default::default()
                },
            ))
            .with_children(|parent| {
                parent.spawn((
                    Enemy {},
                    Gun {
                        direction,
                        bullet_speed: 1.,
                    },
                    AnimationPlayer::default(),
                    Name::new("gun"),
                    Box::from_grid(0., -1.).color(Color::BLUE),
                ));
                parent.spawn((Enemy {}, Box::from_grid(0., 0.)));
            });

        let direction = Vec3::new(0., -1., 0.);
        let position = Vec3::X * GridPosition::SIZE * -10. + Vec3::Y * GridPosition::SIZE * -30.;
        commands.spawn(MaterialMesh2dBundle {
            mesh: meshes
                .add(Mesh::from(LineMarker {
                    start: Vec3::ZERO,
                    end: Vec3::new(3., 0., 0.),
                }))
                .into(),
            transform: Transform::from_xyz(0.5, 0.0, 0.0),
            material: materials.add(LineMaterial { color: Color::BLUE }),
            ..default()
        });
        commands
            .spawn((
                Seeker {
                    velocity: direction,
                    prediction_mult: 1,
                },
                MaterialMeshBundle {
                    mesh: meshes.add(Mesh::from(LineMarker {
                        start: Vec3::ZERO,
                        end: direction * GridPosition::SIZE * 2.,
                    })),
                    material: materials.add(LineMaterial { color: Color::BLUE }),
                    transform: Transform::from_translation(position),
                    ..Default::default()
                },
            ))
            .with_children(|parent| {
                parent.spawn((
                    Enemy {},
                    Gun {
                        direction,
                        bullet_speed: 2.,
                    },
                    AnimationPlayer::default(),
                    Name::new("gun"),
                    Box::from_grid(0., -1.).color(Color::BLUE),
                ));
                parent.spawn((Enemy {}, Box::from_grid(0., 0.)));
            });
    }
}

#[derive(Component)]
struct Seeker {
    velocity: Vec3, // z is not used
    prediction_mult: u8,
}

fn move_seekers(
    time: Res<Time>,
    paused: Res<PauseState>,
    ship: Query<(&Transform, &Ship)>,
    mut seekers: Query<(&mut Transform, &mut Seeker, &Children), Without<Ship>>,
    mut guns: Query<&mut Gun>,
) {
    if paused.0 {
        return;
    }

    let ship = ship.single();
    let predicted_velocity = ship.0.translation - ship.1.previous_position;
    for (mut transform, mut seeker, children) in seekers.iter_mut() {
        let target = ship.0.translation + predicted_velocity * seeker.prediction_mult as f32;
        let desired_velocity = (transform.translation - target).normalize();
        let steering = desired_velocity - seeker.velocity;

        // add steering to the velocity
        // steering is a force, mass and acceleration are equal to 1
        seeker.velocity += steering;

        let direction = seeker.velocity;

        // why this has a minus? who knows!
        transform.translation -= direction * time.delta_seconds() * 20.;

        let forward_enemy = (transform.rotation * Vec3::Y).xy();

        // get the dot product between the enemy forward vector and the direction to the player.
        let forward_dot_player = forward_enemy.dot(direction.xy());

        // if the dot product is approximately 1.0 then the enemy is already facing the player and
        // we can early out.
        if (forward_dot_player - 1.0).abs() < f32::EPSILON {
            continue;
        }

        // get the right vector of the enemy ship in 2D (already unit length)
        let enemy_right = (transform.rotation * Vec3::X).xy();

        // get the dot product of the enemy right vector and the direction to the player ship.
        // if the dot product is negative them we need to rotate counter clockwise, if it is
        // positive we need to rotate clockwise. Note that `copysign` will still return 1.0 if the
        // dot product is 0.0 (because the player is directly behind the enemy, so perpendicular
        // with the right vector).
        let right_dot_player = enemy_right.dot(direction.xy());

        // determine the sign of rotation from the right dot player. We need to negate the sign
        // here as the 2D bevy co-ordinate system rotates around +Z, which is pointing out of the
        // screen. Due to the right hand rule, positive rotation around +Z is counter clockwise and
        // negative is clockwise.
        let rotation_sign = -f32::copysign(1.0, right_dot_player);

        // limit rotation so we don't overshoot the target. We need to convert our dot product to
        // an angle here so we can get an angle of rotation to clamp against.
        let max_angle = forward_dot_player.clamp(-1.0, 1.0).acos(); // clamp acos for safety

        // calculate angle of rotation with limit
        let rotation_angle = rotation_sign * (time.delta_seconds() * 1.).min(max_angle);
        // let rotation_angle = rotation_sign;

        // rotate the enemy to face the player
        transform.rotate_z(rotation_angle);

        let rotation = Quat::from_rotation_z(rotation_angle);
        for child in children {
            if let Ok(mut gun) = guns.get_mut(*child) {
                gun.direction = rotation * gun.direction;
            }
        }
    }
}

#[derive(Component)]
struct PlayerHealthDisplay;

#[derive(Resource)]
struct PartSprites {
    plus: Handle<Image>,
    dot: Handle<Image>,
    cross: Handle<Image>,
}

impl PartSprites {
    fn for_part(&self, part: PartType) -> Handle<Image> {
        match part {
            PartType::Base => self.plus.clone_weak(),
            PartType::Gun(_) => self.dot.clone_weak(),
            PartType::None | PartType::Connector | PartType::Shield => Handle::default(),
        }
    }
}

fn setup(mut commands: Commands, asset_server: ResMut<AssetServer>) {
    let mut gun_animation = AnimationClip::default();
    gun_animation.add_curve_to_path(
        EntityPath {
            parts: vec![Name::new("gun")],
        },
        VariableCurve {
            keyframe_timestamps: vec![0.0, 0.1, 0.2],
            keyframes: Keyframes::Scale(vec![Vec3::ONE, Vec3::splat(1.7), Vec3::ONE]),
        },
    );
    commands.insert_resource(GunAnimation(asset_server.add(gun_animation)));

    commands.insert_resource(PartSprites {
        plus: asset_server.load::<Image>("sprites/plus.png"),
        dot: asset_server.load::<Image>("sprites/dot.png"),
        cross: asset_server.load::<Image>("sprites/cross.png"),
    });

    commands.spawn(Camera2dBundle::default());
}

fn health_bar_update(health: Res<Health>, mut query: Query<&mut Text, With<PlayerHealthDisplay>>) {
    for mut disp in query.iter_mut() {
        disp.sections[0].value = format!("{}", health.0);
    }
}

fn setup_ui(mut commands: Commands, part_sprites: Res<PartSprites>) {
    use Val::*;
    commands.spawn((
        TextBundle {
            text: Text {
                sections: vec![TextSection {
                    value: "00".into(),
                    style: TextStyle {
                        font_size: GridPosition::SIZE * 2.,
                        ..Default::default()
                    },
                }],
                alignment: TextAlignment::Center,
                ..Default::default()
            },
            focus_policy: FocusPolicy::Block,
            ..Default::default()
        },
        PlayerHealthDisplay,
    ));

    commands
        .spawn((
            NodeBundle {
                style: Style {
                    display: Display::Flex,
                    position_type: PositionType::Absolute,
                    border: UiRect::all(Px(GridPosition::SIZE)),
                    padding: UiRect::all(Px(GridPosition::SIZE)),
                    left: Percent(30.),
                    top: Percent(45.),
                    ..Default::default()
                },
                border_color: Color::rgb(0.25, 0.25, 0.25).into(),
                background_color: NORMAL_BUTTON.into(),
                visibility: Visibility::Hidden,
                ..Default::default()
            },
            NewLevelPopup,
        ))
        .with_children(|popup| {
            popup.spawn(TextBundle {
                text: Text::from_section(
                    "New Level!",
                    TextStyle {
                        font_size: GridPosition::SIZE * 4.,
                        ..Default::default()
                    },
                ),
                visibility: Visibility::Inherited,
                ..Default::default()
            });
        });

    commands
        // root node
        .spawn(NodeBundle {
            style: Style {
                display: Display::Grid,
                width: Vw(100.),
                height: Vh(100.),
                grid_template_columns: vec![RepeatedGridTrack::fr(1, 3.)],
                ..Default::default()
            },
            ..Default::default()
        })
        .with_children(|root| {
            root
                // side panel
                .spawn(NodeBundle {
                    style: Style {
                        display: Display::Grid,
                        width: Px(GridPosition::SIZE * (15. * 2. + 3.)),
                        height: Vh(100.),
                        border: UiRect::left(Px(GridPosition::SIZE)),
                        grid_column: GridPlacement::start(3),
                        grid_template_rows: vec![
                            RepeatedGridTrack::percent(1, 55.),
                            RepeatedGridTrack::percent(1, 35.),
                            RepeatedGridTrack::percent(1, 10.),
                        ],
                        ..Default::default()
                    },
                    border_color: Color::rgb(0.25, 0.25, 0.25).into(),
                    background_color: Color::rgb(0.16796875, 0.171875, 0.18359375).into(),
                    ..Default::default()
                })
                .with_children(|side_panel| {
                    // map
                    side_panel.spawn(NodeBundle {
                        style: Style {
                            border: UiRect::all(Px(1.)),
                            ..Default::default()
                        },
                        ..Default::default()
                    });
                    side_panel
                        // ship container
                        .spawn(NodeBundle {
                            style: Style {
                                padding: UiRect {
                                    left: Auto,
                                    right: Auto,
                                    top: Auto,
                                    bottom: Auto,
                                },
                                border: UiRect::top(Px(GridPosition::SIZE)),
                                ..Default::default()
                            },
                            border_color: Color::rgb(0.25, 0.25, 0.25).into(),
                            ..Default::default()
                        })
                        .with_children(|ship_container| {
                            ship_container
                                // ship
                                .spawn((
                                    NodeBundle {
                                        style: Style {
                                            padding: UiRect::all(Px(GridPosition::SIZE)),
                                            display: Display::Grid,
                                            grid_template_rows: vec![RepeatedGridTrack::px(
                                                Ship::MAX_HEIGHT as i32,
                                                GridPosition::SIZE * 2.,
                                            )],
                                            grid_template_columns: vec![RepeatedGridTrack::px(
                                                Ship::MAX_WIDTH as i32,
                                                GridPosition::SIZE * 2.,
                                            )],
                                            ..Default::default()
                                        },
                                        border_color: Color::rgb(0.25, 0.25, 0.25).into(),
                                        ..Default::default()
                                    },
                                    ShipUi { part_count: 2 },
                                ));
                        });

                    // Level and ship part types
                    side_panel
                        .spawn(NodeBundle {
                            // background_color: Color::BLUE.into(),
                            style: Style {
                                display: Display::Flex,
                                flex_direction: FlexDirection::Row,
                                justify_content: JustifyContent::SpaceAround,
                                border: UiRect::top(Px(GridPosition::SIZE)),
                                align_items: AlignItems::Center,
                                ..Default::default()
                            },
                            border_color: Color::rgb(0.25, 0.25, 0.25).into(),
                            ..Default::default()
                        })
                        .with_children(|parent| {
                            parent.spawn((
                                TextBundle {
                                    style: Style {
                                        border: UiRect::left(Px(GridPosition::SIZE)),
                                        ..Default::default()
                                    },
                                    text: Text {
                                        sections: vec![TextSection {
                                            value: "00".into(),
                                            style: TextStyle {
                                                font_size: GridPosition::SIZE * 2.,
                                                ..Default::default()
                                            },
                                        }],
                                        alignment: TextAlignment::Center,
                                        ..Default::default()
                                    },
                                    focus_policy: FocusPolicy::Block,
                                    ..Default::default()
                                },
                                LevelUi {},
                            ));
                            let parts = vec![
                                (
                                    PartButton::Delete,
                                    PartType::None.color(),
                                    part_sprites.cross.clone_weak(),
                                ),
                                (
                                    PartButton::Shield,
                                    PartType::Shield.color(),
                                    Handle::default(),
                                ),
                                (
                                    PartButton::Base,
                                    PartType::Base.color(),
                                    part_sprites.plus.clone_weak(),
                                ),
                                (
                                    PartButton::Gun,
                                    PartType::Gun(Vec2::default()).color(),
                                    part_sprites.dot.clone_weak(),
                                ),
                            ];
                            for (button, color, sprite) in parts.into_iter() {
                                parent.spawn((
                                    ButtonBundle {
                                        style: Style {
                                            border: UiRect::all(Px(2.)),
                                            width: Px(GridPosition::SIZE * 2.),
                                            height: Px(GridPosition::SIZE * 2.),
                                            ..Default::default()
                                        },
                                        image: sprite.into(),
                                        background_color: color.into(),
                                        border_color: color.into(),
                                        ..Default::default()
                                    },
                                    button,
                                ));
                            }
                        });
                });
        });
}

#[derive(Component)]
enum PartButton {
    Delete,
    Shield,
    Base,
    Gun,
}

#[derive(Component)]
struct NewLevelPopup;

#[derive(Component)]
struct LevelUi {}

fn update_level_ui(
    level: Query<&Level, (Changed<Level>, With<Ship>)>,
    mut ui: Query<&mut Text, With<LevelUi>>,
) {
    let Ok(level) = level.get_single() else {
        return;
    };
    ui.single_mut().sections[0].value = format!("{:02}", level.level.clamp(0, 99));
}

// #[derive(Component)]
// struct LevelProgressUi {}

#[derive(Component)]
struct ShipPartUi {
    ship_part: Entity,
    part_type: PartType,
}

#[derive(Component)]
struct ShipUi {
    part_count: usize,
}

const NORMAL_BUTTON: Color = Color::rgb(0.16796875, 0.171875, 0.18359375);
const HOVERED_BUTTON: Color = Color::rgb(0.25, 0.25, 0.25);
const PRESSED_BUTTON: Color = Color::rgb(0.35, 0.75, 0.35);

fn ship_ui_manager(
    paused: Res<PauseState>,

    mut interaction_query: Query<
        (
            &Interaction,
            &mut BackgroundColor,
            &mut BorderColor,
            &mut UiImage,
            &mut ShipPartUi,
        ),
        (Changed<Interaction>, With<Button>, Without<ShipPart>),
    >,

    mut ship_ui_state: Query<&mut ShipUi>,

    mut ship_parts: Query<
        (&mut Sprite, &mut Handle<Image>, &mut Visibility, &ShipPart),
        Without<Button>,
    >,
    part_sprites: Res<PartSprites>,
) {
    if !paused.0 {
        return;
    }

    for (interaction, mut button_color, mut button_border, mut button_image, mut ship_part_ui) in
        &mut interaction_query
    {
        let (mut sprite, mut texture, mut visibility, part) = ship_parts
            .get_mut(ship_part_ui.ship_part)
            .expect("ship part missing");
        info!(?part);
        match *interaction {
            Interaction::Pressed => {
                let next_part = match ship_part_ui.part_type {
                    PartType::None => PartType::Base,
                    PartType::Base => PartType::Shield,
                    PartType::Shield => PartType::Connector,
                    PartType::Connector => PartType::None,
                    PartType::Gun(_) => PartType::None,
                };

                ship_part_ui.part_type = next_part;
                info!(?ship_part_ui.part_type);

                if ship_part_ui.part_type == PartType::None {
                    *visibility = Visibility::Hidden;
                } else {
                    *visibility = Visibility::Visible;
                }

                *texture = part_sprites.for_part(ship_part_ui.part_type);
                *button_image = part_sprites.for_part(ship_part_ui.part_type).into();

                let new_color = ship_part_ui.part_type.color();

                sprite.color = new_color;
                *button_color = new_color.into();
                *button_border = PRESSED_BUTTON.into();
            }
            Interaction::Hovered => {
                *button_border = HOVERED_BUTTON.into();
            }
            Interaction::None => {
                *button_border = button_color.0.into();
            }
        }
    }
}

// #[allow(unused)]
// fn draw_grid(mut gizmos: Gizmos, ui: Query<&ShipUi>) {
//     let window = window.single();
//     let height = (window.height() / GridPosition::SIZE) as i32 / 2;
//     let width = (window.width() / GridPosition::SIZE) as i32 / 2;
//
//     for i in -height..height {
//         for j in -width..(width / 3) {
//             gizmos.rect(
//                 Vec3 {
//                     x: j as f32 * GridPosition::SIZE,
//                     y: i as f32 * GridPosition::SIZE,
//                     z: 0.,
//                 },
//                 Quat::default(),
//                 Vec2::splat(GridPosition::SIZE),
//                 Color::BLACK,
//             )
//         }
//     }
// }
