package spreadsheet.mapper.w2o.process;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 2017/1/4.
 */
public class DefaultWorkbookProcessHelper implements WorkbookProcessHelper {

  private List<SheetProcessHelper> sheetProcessHelpers = new ArrayList<>();

  @Override
  public WorkbookProcessHelper addSheetProcessHelper(SheetProcessHelper sheetProcessHelper) {
    if (sheetProcessHelpers == null) {
      throw new WorkbookProcessException("sheet process helper can not be null");
    }

    sheetProcessHelpers.add(sheetProcessHelper);
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
