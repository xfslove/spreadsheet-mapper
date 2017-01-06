package spreadsheet.mapper.w2o.processor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2017/1/4.
 */
public class DefaultWorkbookProcessHelper implements WorkbookProcessHelper {

  private List<SheetProcessHelper> sheetProcessHelpers = new ArrayList<>();

  @Override
  public WorkbookProcessHelper sheetProcessor(SheetProcessHelper... sheetProcessHelpers) {
    if (sheetProcessHelpers == null) {
      return this;
    }
    Collections.addAll(this.sheetProcessHelpers, sheetProcessHelpers);
    return this;
  }

  @Override
  public List<List> process() {
    List<List> objects = new ArrayList<>();

    for (SheetProcessHelper sheetProcessHelper : sheetProcessHelpers) {

      objects.add(sheetProcessHelper.process());
    }

    return objects;
  }
}
