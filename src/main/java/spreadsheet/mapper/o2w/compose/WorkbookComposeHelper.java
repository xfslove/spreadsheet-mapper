package spreadsheet.mapper.o2w.compose;

import spreadsheet.mapper.model.core.Workbook;

/**
 * workbook compose helper, simply adapter of sheet compose helper
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookComposeHelper {

  /**
   * @param sheetComposeHelper {@link SheetComposeHelper}
   * @return {@link WorkbookComposeHelper}
   */
  WorkbookComposeHelper addSheetComposeHelper(SheetComposeHelper sheetComposeHelper);

  /**
   * @return {@link Workbook}
   * @see SheetComposeHelper#compose()
   */
  Workbook compose();
}
