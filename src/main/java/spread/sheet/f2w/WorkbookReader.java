package spread.sheet.f2w;

import spread.sheet.model.core.Workbook;

import java.io.InputStream;

/**
 * Created by hanwen on 2016/12/30.
 */
public interface WorkbookReader {

  /**
   * read supplied stream to {@link Workbook}
   *
   * @param inputStream auto close
   * @return workbook
   */
  Workbook read(InputStream inputStream);
}
