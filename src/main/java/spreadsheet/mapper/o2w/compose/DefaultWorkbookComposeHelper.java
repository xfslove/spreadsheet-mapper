package spreadsheet.mapper.o2w.compose;

import spreadsheet.mapper.model.core.Workbook;
import spreadsheet.mapper.model.core.WorkbookBean;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.meta.WorkbookMeta;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 2017/1/4.
 */
public class DefaultWorkbookComposeHelper implements WorkbookComposeHelper {

  private List<SheetComposeHelper> sheetComposeHelpers = new ArrayList<>();

  @Override
  public WorkbookComposeHelper addSheetComposeHelper(SheetComposeHelper sheetComposeHelper) {
    if (sheetComposeHelper == null) {
      throw new WorkbookComposeException("sheet compose helper can not be null");
    }

    sheetComposeHelpers.add(sheetComposeHelper);
    return this;
  }

  @Override
  public Workbook compose(List<List> dataOfSheets, WorkbookMeta workbookMeta) {
    int sizeOfData = dataOfSheets.size();
    int sizeOfSheetMetas = workbookMeta.sizeOfSheetMetas();
    int sizeOfHelper = sheetComposeHelpers.size();

    if (sizeOfData != sizeOfSheetMetas) {
      throw new IllegalArgumentException("data's size[" + sizeOfData + "] not equals workbook meta's sheet meta size[" + sizeOfSheetMetas + "]");
    }
    if (sizeOfHelper != sizeOfData) {
      throw new IllegalArgumentException("data's size[" + sizeOfData + "] not equals sheet compose helper size[" + sizeOfHelper + "]");
    }

    Workbook workbook = new WorkbookBean();

    for (int i = 1; i <= sizeOfData; i++) {

      SheetComposeHelper sheetComposeHelper = sheetComposeHelpers.get(i - 1);
      List data = dataOfSheets.get(i - 1);
      SheetMeta sheetMeta = workbookMeta.getSheetMeta(i);

      //noinspection unchecked
      workbook.addSheet(sheetComposeHelper.compose(data, sheetMeta));
    }

    return workbook;
  }
}
