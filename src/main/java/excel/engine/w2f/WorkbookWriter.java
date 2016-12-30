package excel.engine.w2f;

import excel.engine.model.core.Workbook;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Created by hanwen on 2016/12/30.
 */
public interface WorkbookWriter {

  /**
   * write workbook to supplied output stream
   *
   * @param workbook     write workbook
   * @param outputStream notice close the stream
   * @throws IOException io exception
   */
  void write(Workbook workbook, OutputStream outputStream) throws IOException;
}
