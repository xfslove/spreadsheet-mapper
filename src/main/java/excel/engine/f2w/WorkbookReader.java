package excel.engine.f2w;

import excel.engine.model.core.Workbook;

import java.io.IOException;
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
   * @throws IOException io exception
   */
  Workbook read(InputStream inputStream) throws IOException;
}
