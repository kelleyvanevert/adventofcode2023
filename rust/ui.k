
let max = stdin :split "\n\n" :map |line| {  }

Box {
  ScrollView(
    orientation = horizontal,
  ) {
    Image(
      click = {

      }
    )
  }

  VirtualizedScrollView(
    orientation = horizontal
    header = {

    }
    render = {
      //
    }
  )
}

fn VirtualizedScrollView[Item](
  orientation: Orientation = horizontal
  header: Fn[]?
  {..} render: Fn[Item]
) {
  
}
