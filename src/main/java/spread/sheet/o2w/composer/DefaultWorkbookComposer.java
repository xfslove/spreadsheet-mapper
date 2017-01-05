package spread.sheet.o2w.composer;

import spread.sheet.model.core.Workbook;
import spread.sheet.model.core.WorkbookBean;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2017/1/4.
 */
public class DefaultWorkbookComposer implements WorkbookComposer {

  private List<SheetComposer> sheetComposers = new ArrayList<>();

  @Override
  public WorkbookComposer sheetComposer(SheetComposer... sheetComposers) {
    if (sheetComposers == null) {
      return this;
    }
    Collections.addAll(this.sheetComposers, sheetComposers);
    return this;
  }

  public Workbook compose() {

    Workbook workbook = new WorkbookBean();

    for (SheetComposer sheetComposer : sheetComposers) {
      workbook.addSheet(sheetComposer.compose());
    }

    return workbook;
  }
}
