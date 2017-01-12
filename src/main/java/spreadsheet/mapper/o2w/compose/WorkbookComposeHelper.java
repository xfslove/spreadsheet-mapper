package spreadsheet.mapper.o2w.compose;

import spreadsheet.mapper.model.core.Workbook;

/**
 * workbook compose helper, simply adapter of sheet compose helper
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookComposeHelper {

  /**
   * @param sheetComposeHelpers {@link SheetComposeHelper}
   * @return {@link WorkbookComposeHelper}
   */
  WorkbookComposeHelper sheetComposes(SheetComposeHelper... sheetComposeHelpers);

  /**
   * @return {@link Workbook}
   * @see SheetComposeHelper#compose()
   */
  Workbook compose();
}
