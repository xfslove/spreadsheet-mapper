package spreadsheet.mapper.o2w.composer;

import spreadsheet.mapper.model.core.Workbook;

/**
 * workbook composer, simply adapter of sheet composer
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookComposeHelper {

  /**
   * @param sheetComposeHelpers {@link SheetComposeHelper}
   */
  WorkbookComposeHelper sheetComposer(SheetComposeHelper... sheetComposeHelpers);

  /**
   * @return {@link Workbook}
   * @see SheetComposeHelper#compose()
   */
  Workbook compose();
}
