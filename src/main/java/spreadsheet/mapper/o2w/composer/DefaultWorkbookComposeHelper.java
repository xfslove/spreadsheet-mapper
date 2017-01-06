package spreadsheet.mapper.o2w.composer;

import spreadsheet.mapper.model.core.Workbook;
import spreadsheet.mapper.model.core.WorkbookBean;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2017/1/4.
 */
public class DefaultWorkbookComposeHelper implements WorkbookComposeHelper {

  private List<SheetComposeHelper> sheetComposeHelpers = new ArrayList<>();

  @Override
  public WorkbookComposeHelper sheetComposer(SheetComposeHelper... sheetComposeHelpers) {
    if (sheetComposeHelpers == null) {
      return this;
    }
    Collections.addAll(this.sheetComposeHelpers, sheetComposeHelpers);
    return this;
  }

  public Workbook compose() {

    Workbook workbook = new WorkbookBean();

    for (SheetComposeHelper sheetComposeHelper : sheetComposeHelpers) {
      workbook.addSheet(sheetComposeHelper.compose());
    }

    return workbook;
  }
}
