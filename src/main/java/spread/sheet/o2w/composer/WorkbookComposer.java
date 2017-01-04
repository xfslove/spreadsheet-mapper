package spread.sheet.o2w.composer;

import spread.sheet.model.core.Workbook;

/**
 * workbook composer
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookComposer {

  /**
   * @param sheetComposers sheet composer
   * @see SheetComposer
   */
  WorkbookComposer sheetComposer(SheetComposer... sheetComposers);

  /**
   * @return composed workbook
   * @see SheetComposer#compose()
   */
  Workbook compose();
}
